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
    val reader = new FileReader(yamlFilename)
    val yamlLoader = new Yaml(new SafeConstructor)
    val javaSrc = yamlLoader.load(reader)
    val scalaSrc = yamlJavaToScala(javaSrc)
    val spec = ClassSpec.fromYaml(scalaSrc, List())
    TypeProcessor.processTypes(spec)
    spec
  }

  def yamlJavaToScala(src: AnyRef): Any = {
    src match {
      case jlist: JList[AnyRef] =>
        jlist.toList.map(yamlJavaToScala)
      case jmap: JMap[String, AnyRef] =>
        jmap.toMap.mapValues(yamlJavaToScala)
      case _: String =>
        src
      case javaInt: java.lang.Integer =>
        javaInt.intValue
    }
  }
}
