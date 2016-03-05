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
  val desc: ClassSpec = mapper.readValue(reader, classOf[ClassSpec])
  val endian: Option[String] = desc.meta.get("endian")
  val topClassName = desc.meta("id")

  val userTypes = gatherUserTypes(desc) ++ Map(topClassName -> desc)
  markupParentTypes(topClassName, desc)

  var nowClassName: String = topClassName
  var nowClass: ClassSpec = desc

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
                throw new RuntimeException("type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
            }
          }
        case _ => // ignore, it's standard type
      }
    }
  }

  def compile {
    lang.open(topClassName, this)
    lang.fileHeader(yamlFilename, topClassName)
    compileClass(topClassName, desc)
    lang.fileFooter(topClassName)
    lang.close
  }

  def compileClass(name: String, curClass: ClassSpec): Unit = {
    nowClass = curClass
    nowClassName = name

    lang.classHeader(name)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec.create(id = "_root", dataType = topClassName)
    extraAttrs += AttrSpec.create(id = "_parent", dataType = curClass.parentTypeName)

    lang.classConstructorHeader(name, curClass.parentTypeName, topClassName)
    curClass.seq.foreach((attr) => compileAttribute(attr, attr.id, extraAttrs))
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

  def compileAttribute(attr: AttrSpec, id: String, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    attr.dataType match {
      case FixedBytesType(c) =>
        lang.attrFixedContentsParse(id, c)
      case t: UserType =>
        val newIO = if (compileAttributeNoType(attr, s"_raw_${id}")) {
          // we have a fixed buffer, thus we shall create separate IO for it
          // FIXME: technically, should bear something CalcBytesType
          extraAttrs += AttrSpec.create(s"_raw_${id}", sizeEos = true)
          lang.allocateIO(s"_raw_${id}")
        } else {
          lang.normalIO
        }
        lang.attrUserTypeParse(id, t, attr, newIO)
      case _: BytesType =>
        // use intermediate variable name, if we'll be doing post-processing
        val rawId = attr.process match {
          case None => id
          case Some(_) =>
            // FIXME: technically, should bear something CalcBytesType
            extraAttrs += AttrSpec.create(s"_raw_${id}", sizeEos = true)
            s"_raw_${id}"
        }

        if (!compileAttributeNoType(attr, rawId)) {
          throw new RuntimeException("no type encountered and bad size / size_eos spec")
        }

        // apply post-processing
        attr.process.foreach((proc) => lang.attrProcess(proc, rawId, id))
      case _ =>
        lang.attrStdTypeParse(id, attr, endian)
    }
  }

  def compileAttributeNoType(attr: AttrSpec, id: String): Boolean = {
    attr.dataType match {
      case BytesLimitType(_) | UserTypeByteLimit(_, _) =>
        lang.attrNoTypeWithSize(id, attr)
        // TODO: call postprocess here
        true
      case BytesEosType | UserTypeEos(_) =>
        lang.attrNoTypeWithSizeEos(id, attr)
        // TODO: call postprocess here
        true
      case _ =>
        false
    }
  }

  def compileInstance(className: String, instName: String, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Determine datatype
    val dataType = instSpec.value match {
      case Some(e: Ast.expr) => lang.translator.detectType(e)
      case None => instSpec.dataType
    }

    // Memorize it for further use
    instSpec.calcDataType = dataType

    // Declare caching variable
    lang.instanceDeclaration(instName, dataType, instSpec.isArray)

    lang.instanceHeader(className, instName, dataType, instSpec.isArray)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec.value match {
      case Some(e: Ast.expr) =>
        lang.instanceCalculate(instName, e)
      case None =>
        // TODO: "inside" support
        instSpec.positionAbs.foreach((pos) => lang.seek(lang.normalIO, pos))
        compileAttribute(instSpec, lang.instanceAttrName(instName), extraAttrs)
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
        classSpec.instances.foreach(instances => instances.foreach {
          case(instName: String, inst: InstanceSpec) =>
            if (instName == attrName)
              return inst.dataTypeComposite
        })
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
