package io.kaitai.struct

import java.io.FileReader
import java.nio.charset.Charset
import java.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.LanguageCompiler
import io.kaitai.struct.translators.TypeProvider

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class ClassCompiler(val yamlFilename: String, val lang: LanguageCompiler) extends TypeProvider {
  val reader = new FileReader(yamlFilename)
  val mapper = new ObjectMapper(new YAMLFactory())
  val topClass: ClassSpec = mapper.readValue(reader, classOf[ClassSpec])
  val endian: Option[String] = topClass.meta.get("endian")
  val topClassName = topClass.meta("id")

  val userTypes = gatherUserTypes(topClass) ++ Map(topClassName -> topClass)

  var nowClassName: String = topClassName
  var nowClass: ClassSpec = topClass

  def gatherUserTypes(curClass: ClassSpec): Map[String, ClassSpec] = {
    curClass.types match {
      case Some(typeMap) =>
        val curValues: Map[String, ClassSpec] = typeMap
        val recValues: Map[String, ClassSpec] = typeMap.map {
          case (typeName, intClass) => gatherUserTypes(intClass)
        }.flatten.toMap
        curValues ++ recValues
      case None => Map()
    }
  }

  def markupParentTypes(curClassName: String, curClass: ClassSpec): Unit = {
    curClass.seq.foreach { attr =>
      attr.dataType match {
        case userType: UserType =>
          val ut = userType.name
          userTypes.get(ut).foreach { usedClass =>
            usedClass._parentType match {
              case None =>
                usedClass._parentType = Some((curClassName, curClass))
                markupParentTypes(ut, usedClass)
              case Some((curClassName, curClass)) => // already done, don't do anything
              case Some((otherName, otherClass)) =>
                throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
            }
          }
        case _ => // ignore, it's standard type
      }
    }
  }

  def deriveValueTypes {
    userTypes.foreach { case (name, spec) => deriveValueType(spec) }
  }

  def deriveValueType(curClass: ClassSpec): Unit = {
    nowClass = curClass
    curClass.instances.foreach { realInstances => realInstances.foreach {
      case (instName, inst) =>
        inst match {
          case vi: ValueInstanceSpec =>
            vi.dataType = Some(lang.translator.detectType(vi.value))
          case _ =>
            // do nothing
        }
    }}
  }

  def compile {
    lang.open(topClassName, this)

    deriveValueTypes
    markupParentTypes(topClassName, topClass)

    lang.fileHeader(yamlFilename, topClassName)
    compileClass(topClassName, topClass)
    lang.fileFooter(topClassName)
    lang.close
  }

  def compileClass(name: String, curClass: ClassSpec): Unit = {
    nowClass = curClass
    nowClassName = name

    lang.classHeader(name)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec("_root", UserTypeInstream(topClassName))
    extraAttrs += AttrSpec("_parent", UserTypeInstream(curClass.parentTypeName))

    lang.classConstructorHeader(name, curClass.parentTypeName, topClassName)
    curClass.seq.foreach((attr) => lang.attrParse(attr, attr.id, extraAttrs, lang.normalIO))
    lang.classConstructorFooter

    // Recursive types
    curClass.types.foreach((typeMap) => typeMap.foreach {
      case (typeName, intClass) => compileClass(typeName, intClass)
    })

    nowClass = curClass
    nowClassName = name

    curClass.instances.foreach((instanceMap) => instanceMap.foreach {
      case (instName, instSpec) => compileInstance(name, instName, instSpec, extraAttrs)
    })

    // Attributes declarations and readers
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataType, attr.isArray))
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeReader(attr.id, attr.dataType, attr.isArray))

    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    lang.classFooter(name)
  }

  def compileInstance(className: String, instName: String, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Determine datatype
    val (dataType, isArray) = instSpec match {
      case t: ValueInstanceSpec => (t.dataType.get, false)
      case t: ParseInstanceSpec => (t.dataType, t.isArray)
    }

    // Declare caching variable
    lang.instanceDeclaration(instName, dataType, isArray)

    lang.instanceHeader(className, instName, dataType, isArray)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec match {
      case ValueInstanceSpec(value, _) => lang.instanceCalculate(instName, value)
      case i: ParseInstanceSpec =>
        // TODO: "inside" support
        i.positionAbs.foreach((pos) => lang.seek(lang.normalIO, pos))
        lang.attrParse(i, lang.instanceAttrName(instName), extraAttrs, lang.normalIO)
    }

    lang.instanceReturn(instName)
    lang.instanceFooter
  }

  override def determineType(attrName: String): BaseType = {
    determineType(nowClass, nowClassName, attrName)
  }

  override def determineType(typeName: String, attrName: String): BaseType = {
    getTypeByName(nowClass, typeName) match {
      case Some(t) => determineType(t, typeName, attrName)
      case None => throw new RuntimeException(s"Unable to determine type for ${attrName} in type ${typeName}")
    }
  }

  def determineType(classSpec: ClassSpec, className: String, attrName: String): BaseType = {
    attrName match {
      case "_root" =>
        UserTypeInstream(topClassName)
      case "_parent" =>
        UserTypeInstream(classSpec.parentTypeName)
      case _ =>
        classSpec.seq.foreach { el =>
          if (el.id == attrName)
            return el.dataTypeComposite
        }
        classSpec.instances.foreach(instances =>
          instances.get(attrName) match {
            case Some(i: ValueInstanceSpec) => return i.dataType.get
            case Some(i: ParseInstanceSpec) => return i.dataTypeComposite
            case None => // do nothing
          }
        )
        throw new RuntimeException(s"Unable to access ${attrName} in ${className} context")
    }
  }

  def getTypeByName(inClass: ClassSpec, name: String): Option[ClassSpec] = {
    userTypes.get(name)

    // Some special code to support non-unique type names lookup - might come useful in future
//    if (name == topClassName)
//      return Some(desc)
//
//    if (inClass.types.isEmpty)
//      return None
//
//    val types = inClass.types.get
//    val r = types.get(name) match {
//      case Some(x) => Some(x)
//      case None => {
//        types.foreach { case (inTypesKey, inTypesValue) =>
//          if (inTypesValue != inClass) {
//            val r = getTypeByName(inTypesValue, name)
//            if (r.isDefined)
//              return r
//          }
//        }
//
//        // Look up global names list
//        userTypes.get(name)
//      }
//    }
  }

  def compileEnum(enumName: String, enumColl: Map[Long, String]): Unit = {
    lang.enumDeclaration(nowClassName, enumName, enumColl)
  }
}
