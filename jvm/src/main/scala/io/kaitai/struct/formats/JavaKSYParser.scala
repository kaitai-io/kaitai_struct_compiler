package io.kaitai.struct.formats

import java.io.FileReader

import io.kaitai.struct.TypeProcessor
import io.kaitai.struct.format.ClassSpec
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

import java.util.{List => JList, Map => JMap}
import scala.collection.JavaConversions._

object JavaKSYParser {
  def localFileToSpec(yamlFilename: String): ClassSpec = {
    val scalaSrc = readerToYaml(new FileReader(yamlFilename))
    val spec = ClassSpec.fromYaml(scalaSrc)
    TypeProcessor.processTypes(spec)
    spec
  }

  def readerToYaml(reader: FileReader): Any = {
    val yamlLoader = new Yaml(new SafeConstructor)
    val javaSrc = yamlLoader.load(reader)
    yamlJavaToScala(javaSrc)
  }

  def stringToYaml(data: String): Any = {
    val yamlLoader = new Yaml(new SafeConstructor)
    val javaSrc = yamlLoader.load(data)
    yamlJavaToScala(javaSrc)
  }

  def yamlJavaToScala(src: Any): Any = {
    src match {
      case jlist: JList[AnyRef] =>
        jlist.toList.map(yamlJavaToScala)
      case jmap: JMap[String, AnyRef] =>
        jmap.toMap.mapValues(yamlJavaToScala)
      case _: String =>
        src
      case _: Double =>
        src
      case _: Boolean =>
        src
      case javaInt: java.lang.Integer =>
        javaInt.intValue
      case null =>
        // may be not the very best idea, but these nulls
        // should be handled by real parsing code, i.e. where
        // it tracks tree depth, etc.
        null
    }
  }
}
