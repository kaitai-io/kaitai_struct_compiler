package io.kaitai.structures

import java.io.FileReader
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.kaitai.structures.format._
import io.kaitai.structures.languages.LanguageCompiler

import collection.JavaConversions._

class ClassCompiler(val yamlFilename: String, val lang: LanguageCompiler) {
  val reader = new FileReader(yamlFilename)
  val mapper = new ObjectMapper(new YAMLFactory())
  val desc: ClassSpec = mapper.readValue(reader, classOf[ClassSpec])
  val userTypes = gatherUserTypes(desc).toSet
  val endian: Option[String] = Option(desc.meta("endian"))

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
    lang.fileHeader(yamlFilename, topClass)
    compileClass(topClass, desc)
  }

  def compileClass(name: String, curClass: ClassSpec): Unit = {
    lang.classHeader(name)

    curClass.seq.foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataType))

    lang.classConstructorHeader(name)
    curClass.seq.foreach((attr) => compileAttribute(attr, attr.id))
    lang.classConstructorFooter

    curClass.seq.foreach((attr) => lang.attributeReader(attr.id, attr.dataType))

    // Recursive types
    curClass.types.foreach((typeMap) => typeMap.foreach {
      case (typeName, intClass) => compileClass(typeName, intClass)
    })

    // TODO: instances

    // TODO: maps

    lang.classFooter
  }

  def compileAttribute(attr: AttrSpec, id: String, ioName: String = "@_io"): Unit = {
    if (attr.contents != null) {
      lang.attrFixedContentsParse(id, parseContentSpec(attr))
    } else {
      if (userTypes.contains(attr.dataType)) {
        // TODO: parse user datatypes
      } else {
        if (ioName != "@_io")
          throw new RuntimeException("unable to read non-standard IO for standard type")
        lang.attrStdTypeParse(attr, endian)
      }
    }
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
