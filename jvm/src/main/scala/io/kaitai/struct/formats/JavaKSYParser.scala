package io.kaitai.struct.formats

import java.io._
import java.nio.charset.StandardCharsets
import java.util.{List => JList, Map => JMap}
import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.problems.{CompilationProblem, CompilationProblemException, ProblemCoords, YAMLParserError}
import io.kaitai.struct.{Log, Main}

import java.nio.file.{Files, Paths}
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object JavaKSYParser {
  def localFileToSpecs(yamlFilename: String, config: CLIConfig): (Option[ClassSpecs], Iterable[CompilationProblem]) = {
    try {
      val firstSpec = fileNameToSpec(yamlFilename)
      val yamlDir = Option(new File(yamlFilename).getParent).getOrElse(".")
      val specs = new JavaClassSpecs(yamlDir, config.importPaths, firstSpec)

      val problems = Await.result(Main.importAndPrecompile(specs, config.runtime), Duration.Inf)
      (Some(specs), problems)
    } catch {
      case ex: CompilationProblemException =>
        if (config.throwExceptions)
          ex.printStackTrace()

        val problem = ex.problem
        val problemLocalized = if (problem.coords.file.isEmpty) {
          problem.localizedInFile(yamlFilename)
        } else {
          problem
        }
        (None, List(problemLocalized))
    }
  }

  def fileNameToSpec(yamlFilename: String): ClassSpec = {
    Log.fileOps.info(() => s"reading $yamlFilename...")

    val yamlStr = new String(Files.readAllBytes(Paths.get(yamlFilename)), StandardCharsets.UTF_8)

//    try {
      val scalaSrc = stringToYaml(yamlStr)
      ClassSpec.fromYaml(scalaSrc, Some(yamlFilename))
//    } catch {
//      case marked: MarkedYAMLException =>
//        val mark = marked.getProblemMark
//        throw CompilationProblemException(
//          YAMLParserError(
//            marked.getProblem,
//            ProblemCoords(
//              Some(yamlFilename),
//              None,
//              Some(mark.getLine + 1),
//              Some(mark.getColumn + 1)
//            )
//          )
//        )
//    }
  }

  def stringToYaml(data: String): yamlesque.Node = yamlesque.read(data)
}
