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
      userTypes.get(attr.dataType).foreach { usedClass =>
        usedClass._parentType match {
          case None =>
            usedClass._parentType = Some((curClassName, curClass))
            markupParentTypes(attr.dataType, usedClass)
          case Some((curClassName, curClass)) => // already done, don't do anything
          case Some((otherName, otherClass)) =>
            throw new RuntimeException("type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
        }
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

    // TODO: maps

    lang.classFooter(name)
  }

  def compileAttribute(attr: AttrSpec, id: String, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    if (attr.contents != null) {
      lang.attrFixedContentsParse(id, parseContentSpec(attr))
    } else {
      if (userTypes.contains(attr.dataType)) {
        val newIO = if (compileAttributeNoType(attr, s"_raw_${id}")) {
          // we have a fixed buffer, thus we shall create separate IO for it
          extraAttrs += AttrSpec.create(s"_raw_${id}")
          lang.allocateIO(s"_raw_${id}")
        } else {
          lang.normalIO
        }
        lang.attrUserTypeParse(id, attr, newIO)
      } else if (attr.dataType == null) {
        // use intermediate variable name, if we'll be doing post-processing
        val rawId = attr.process match {
          case None => id
          case Some(_) =>
            extraAttrs += AttrSpec.create(s"_raw_${id}")
            s"_raw_${id}"
        }

        if (!compileAttributeNoType(attr, rawId)) {
          throw new RuntimeException("no type encountered and bad size / size_eos spec")
        }

        // apply post-processing
        attr.process.foreach((proc) => lang.attrProcess(proc, rawId, id))
      } else {
        lang.attrStdTypeParse(id, attr, endian)
      }
    }
  }

  def compileAttributeNoType(attr: AttrSpec, id: String): Boolean = {
    (attr.size, attr.sizeEos) match {
      case (Some(sizeVar: Ast.expr), false) =>
        lang.attrNoTypeWithSize(id, attr)
        // TODO: call postprocess here
        true
      case (None, true) =>
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
      case Some(e: Ast.expr) =>
        lang.translator.detectType(e) match {
          case IntType => "s4"
          case StrType => "str"
          case BooleanType => "bool"
          case BytesType => null
          case UserType(x) => x
        }
      case None =>
        instSpec.dataType
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

  def parseContentSpec(attr: AttrSpec): Array[Byte] = {
    val c = attr.contents
    if (c.isInstanceOf[String]) {
      c.asInstanceOf[String].getBytes(Charset.forName("UTF-8"))
    } else if (c.isInstanceOf[util.ArrayList[Object]]) {
      val arr = c.asInstanceOf[util.ArrayList[Object]].toList
      val bb = new scala.collection.mutable.ArrayBuffer[Byte]
      arr.foreach((el) =>
        if (el.isInstanceOf[String]) {
          val strBytes = el.asInstanceOf[String].getBytes(Charset.forName("UTF-8"))
          bb.appendAll(strBytes)
        } else if (el.isInstanceOf[Integer]) {
          bb.append(el.asInstanceOf[Integer].toByte)
        } else {
          throw new RuntimeException(s"Unable to parse fixed content in array: ${el}")
        }
      )
      bb.toArray
    } else {
      throw new RuntimeException(s"Unable to parse fixed content: ${c.getClass}")
    }
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
        UserType(topClassName)
      case "_parent" =>
        UserType(classSpec.parentTypeName)
      case _ =>
        classSpec.seq.foreach { el =>
          if (el.id == attrName)
            return el.dataTypeAsBaseType
        }
        classSpec.instances.foreach(instances => instances.foreach {
          case(instName: String, inst: InstanceSpec) =>
            if (instName == attrName)
              return inst.dataTypeAsBaseType
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
}
