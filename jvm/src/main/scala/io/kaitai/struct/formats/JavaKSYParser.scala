package io.kaitai.struct.formats

import java.io.{File, FileReader}
import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.{Log, TypeProcessor}
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

import scala.collection.JavaConversions._

object JavaKSYParser {
  def localFileToSpecs(yamlFilename: String): ClassSpecs = {
    val specs = new JavaClassSpecs(new File(yamlFilename).getParent)

    val firstSpec = fileNameToSpec(yamlFilename)

    TypeProcessor.processTypesMany(specs, firstSpec)
    specs
  }

  def fileNameToSpec(yamlFilename: String): ClassSpec = {
    Log.fileOps.info(() => s"reading $yamlFilename...")
    val scalaSrc = readerToYaml(new FileReader(yamlFilename))
    ClassSpec.fromYaml(scalaSrc)
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
