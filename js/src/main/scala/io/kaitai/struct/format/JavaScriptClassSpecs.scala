package io.kaitai.struct.format

import io.kaitai.struct.JavaScriptImporter
import io.kaitai.struct.problems.ErrorInInput

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext

class JavaScriptClassSpecs(importer: JavaScriptImporter, firstSpec: ClassSpec)
  extends ClassSpecs(firstSpec) {

  val MODE_REL = "rel"
  val MODE_ABS = "abs"

  override def importRelative(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]] =
    doImport(name, path, MODE_REL)
  override def importAbsolute(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]] =
    doImport(name, path, MODE_ABS)

  def doImport(name: String, path: List[String], mode: String): Future[Option[ClassSpec]] = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    importer.importYaml(name, mode).toFuture
      .transform((yaml) => {
        val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
        Some(ClassSpec.fromYaml(yamlScala, Some(name)))
      }, (err) => {
        throw ErrorInInput(err, path).toException
      })
  }
}
