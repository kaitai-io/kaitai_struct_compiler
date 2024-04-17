package io.kaitai.struct.format

import io.kaitai.struct.{JavaScriptImporter, Main, RuntimeConfig}

import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import io.kaitai.struct.problems.ProblemSeverity

object JavaScriptKSYParser {
  /**
    * Converts first YAML (given as JavaScript object) to the ClassSpecs
    * object, fully imported and precompiled.
    * @param yaml first KSY file (YAML), given as JavaScript object
    * @return future of ClassSpecs object
    */
  def yamlToSpecs(yaml: Any, importer: JavaScriptImporter, config: RuntimeConfig): Future[ClassSpecs] = {
    val yamlScala = yamlJavascriptToScala(yaml)
    val firstSpec = ClassSpec.fromYaml(yamlScala, None)
    val specs = new JavaScriptClassSpecs(importer, firstSpec)
    Main.importAndPrecompile(specs, config).map { problems =>
      // throw the first (if any) severe (not a warning) problem as an exception
      problems.find(p => p.severity != ProblemSeverity.Warning) match {
        case Some(problem) => throw problem.toException
        case None => specs
      }
    }
  }

  def yamlJavascriptToScala(src: Any): Any = {
    src match {
      case array: js.Array[AnyRef] =>
        array.toList.map(yamlJavascriptToScala)
      case _: String | _: Int | _: Double | _: Boolean =>
        src
      case dict =>
        dict.asInstanceOf[js.Dictionary[AnyRef]].toMap.view.mapValues(yamlJavascriptToScala).toMap
    }
  }
}
