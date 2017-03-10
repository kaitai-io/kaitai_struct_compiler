package io.kaitai.struct.format

import io.kaitai.struct.JavaScriptImporter

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext

class JavaScriptClassSpecs(importer: JavaScriptImporter) extends ClassSpecs {
  override def importRelative(name: List[String]): Future[Option[ClassSpec]] = {
    importer.importYaml(name.mkString("/")).toFuture.map { (yaml) =>
      val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
      Some(ClassSpec.fromYaml(yamlScala))
    }(JSExecutionContext.queue)
  }
  override def importAbsolute(name: List[String]): Future[Option[ClassSpec]] = ???
}
