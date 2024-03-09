package io.kaitai.struct.formats

import java.io._
import java.nio.charset.StandardCharsets
import java.util.{List => JList, Map => JMap}
import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.problems.{CompilationProblem, CompilationProblemException, ProblemCoords, ProblemSeverity, YAMLParserError}
import io.kaitai.struct.{Log, Main}
import org.yaml.snakeyaml.error.MarkedYAMLException
import org.yaml.snakeyaml.{LoaderOptions, Yaml}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

object JavaKSYParser {
  def localFileToSpecs(yamlFilename: String, config: CLIConfig): (Option[ClassSpecs], Iterable[CompilationProblem]) = {
    try {
      val firstSpec = fileNameToSpec(yamlFilename)
      val yamlDir = Option(new File(yamlFilename).getParent).getOrElse(".")
      val specs = new JavaClassSpecs(yamlDir, config.importPaths, firstSpec)

      val problems = Await.result(Main.importAndPrecompile(specs, config.runtime), Duration.Inf)
      val gotError = problems.exists(p => p.severity != ProblemSeverity.Warning)
      (if (gotError) None else Some(specs), problems)
    } catch {
      case ex: CompilationProblemException =>
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

    // This complex string of classes is due to the fact that Java's
    // default "FileReader" implementation always uses system locale,
    // which screws up encoding on some systems and screws up reading
    // UTF-8 files with BOM
    val fis = new FileInputStream(yamlFilename)
    val isr = new InputStreamReader(fis, StandardCharsets.UTF_8)
    val br = new BufferedReader(isr)
    try {
      val scalaSrc = readerToYaml(br)
      ClassSpec.fromYaml(scalaSrc, Some(yamlFilename))
    } catch {
      case marked: MarkedYAMLException =>
        val mark = marked.getProblemMark
        throw CompilationProblemException(
          YAMLParserError(
            marked.getProblem,
            ProblemCoords(
              Some(yamlFilename),
              None,
              Some(mark.getLine + 1),
              Some(mark.getColumn + 1)
            )
          )
        )
    }
  }

  def getYamlLoader: Yaml = {
    val loaderOptions = new LoaderOptions
    loaderOptions.setAllowDuplicateKeys(false)
    new Yaml(loaderOptions)
  }

  def readerToYaml(reader: Reader): Any = {
    yamlJavaToScala(getYamlLoader.load(reader))
  }

  def stringToYaml(data: String): Any = {
    yamlJavaToScala(getYamlLoader.load(data))
  }

  def yamlJavaToScala(src: Any): Any = {
    src match {
      case jlist: JList[AnyRef] =>
        jlist.asScala.toList.map(yamlJavaToScala)
      case jmap: JMap[String, AnyRef] =>
        jmap.asScala.toMap.view.mapValues(yamlJavaToScala).toMap
      case _: String =>
        src
      case _: Double =>
        src
      case _: Boolean =>
        src
      case javaInt: java.lang.Integer =>
        javaInt.intValue
      case javaLong: java.lang.Long =>
        javaLong.longValue
      case _: java.math.BigInteger =>
        src.toString
      case null =>
        // may be not the very best idea, but these nulls
        // should be handled by real parsing code, i.e. where
        // it tracks tree depth, etc.
        null
    }
  }
}
