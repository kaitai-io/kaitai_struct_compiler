package io.kaitai.struct.format

import io.kaitai.struct.JavaScriptImporter

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext

class JavaScriptClassSpecs(importer: JavaScriptImporter) extends ClassSpecs {
  val MODE_REL = "rel"
  val MODE_ABS = "abs"

  override def importRelative(name: String): Future[Option[ClassSpec]] =
    doImport(name, MODE_REL)
  override def importAbsolute(name: String): Future[Option[ClassSpec]] =
    doImport(name, MODE_ABS)

  def doImport(name: String, mode: String): Future[Option[ClassSpec]] =
    importer.importYaml(name, mode).toFuture.map { (yaml) =>
      val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
      Some(ClassSpec.fromYaml(yamlScala))
    }(JSExecutionContext.queue)
}
