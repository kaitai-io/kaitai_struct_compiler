package io.kaitai.struct

import java.io.FileReader
import java.nio.charset.Charset
import java.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.kaitai.struct.format._
import io.kaitai.struct.languages.LanguageCompiler

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class ClassCompiler(val yamlFilename: String, val lang: LanguageCompiler) {
  val reader = new FileReader(yamlFilename)
  val mapper = new ObjectMapper(new YAMLFactory())
  val desc: ClassSpec = mapper.readValue(reader, classOf[ClassSpec])
  val userTypes = gatherUserTypes(desc).toSet
  val endian: Option[String] = desc.meta.get("endian")

  def gatherUserTypes(curClass: ClassSpec): List[String] = {
    curClass.types match {
      case Some(typeMap) =>
        val curValues: List[String] = typeMap.keys.toList
        val recValues = typeMap.map {
          case (typeName, intClass) => gatherUserTypes(intClass)
        }.flatten
        curValues ++ recValues
      case None => List()
    }
  }

  def compile {
    val topClass = desc.meta("id")
    lang.open(topClass)
    lang.fileHeader(yamlFilename, topClass)
    compileClass(topClass, desc)
    lang.fileFooter(topClass)
    lang.close
  }

  def compileClass(name: String, curClass: ClassSpec): Unit = {
    lang.classHeader(name)

    val extraAttrs = ListBuffer[AttrSpec]()

    lang.classConstructorHeader(name)
    curClass.seq.foreach((attr) => compileAttribute(attr, attr.id, extraAttrs))
    lang.classConstructorFooter

    // Recursive types
    curClass.types.foreach((typeMap) => typeMap.foreach {
      case (typeName, intClass) => compileClass(typeName, intClass)
    })

    curClass.instances.foreach((instanceMap) => instanceMap.foreach {
      case (instName, instSpec) => compileInstance(instName, instSpec, extraAttrs)
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
        lang.attrStdTypeParse(attr, endian)
      }
    }
  }

  def compileAttributeNoType(attr: AttrSpec, id: String): Boolean = {
    (attr.size, attr.sizeEos) match {
      case (Some(sizeVar: String), false) =>
        lang.attrNoTypeWithSize(id, sizeVar)
        // TODO: call postprocess here
        true
      case (None, true) =>
        lang.attrNoTypeWithSizeEos(id)
        // TODO: call postprocess here
        true
      case _ =>
        false
    }
  }

  def compileInstance(instName: String, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Declare caching variable
    lang.attributeDeclaration(instName, instSpec.dataType, instSpec.isArray)

    lang.instanceHeader(instName, instSpec.dataType, instSpec.isArray)
    lang.instanceCheckCacheAndReturn(instName)

    // TODO: "inside" support
    instSpec.positionAbs.foreach((pos) => lang.seek(lang.normalIO, pos))
    compileAttribute(instSpec, lang.instanceAttrName(instName), extraAttrs)

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
}
