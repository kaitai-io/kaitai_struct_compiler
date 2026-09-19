package io.kaitai.struct.languages

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.{Main, RuntimeConfig, Version}
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.sys.process._

class GoProcessedCheckCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
    test("fixed length-preserving processors validate logical lengths during Check") {
      val dir = Files.createTempDirectory("kaitai-go-processed-check-")
      def write(name: String, text: String): Unit = {
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
        ()
      }
      def schema(name: String, body: String): Unit = {
        val path = dir.resolve(s"$name.ksy")
        write(path.getFileName.toString, s"meta:\n  id: $name\n$body")
        val config = RuntimeConfig(readWrite = true, goPackage = "contract")
        val (specsOption, problems) = JavaKSYParser.localFileToSpecs(path.toString, CLIConfig(runtime = config))
        val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
        Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach { file =>
          write(new File(file.fileName).getName, file.contents)
        }
      }

      schema("processed_rotate_bytes", "seq:\n  - id: data\n    size: 5\n    process: rol(1)\n")
      schema("processed_rotate_repeat", "seq:\n  - id: data\n    size: 2\n    process: ror(1)\n    repeat: expr\n    repeat-expr: 2\n")
      schema("processed_xor_optional", "params:\n  - id: enabled\n    type: bool\nseq:\n  - id: data\n    if: enabled\n    size: 2\n    process: xor(1)\n")
      schema("processed_xor_switch", "params:\n  - id: kind\n    type: u1\nseq:\n  - id: data\n    size: 2\n    process: xor(1)\n    type:\n      switch-on: kind\n      cases:\n        1: body\ntypes:\n  body:\n    seq:\n      - id: data\n        size-eos: true\n")
      schema("processed_rotate_padded", "seq:\n  - id: data\n    size: 4\n    pad-right: 255\n    process: rol(1)\n")
      schema("processed_resize_custom", "seq:\n  - id: data\n    size: 2\n    process: resize_codec\n")
      schema("processed_resize_zlib", "params:\n  - id: encoded_size\n    type: u4\nseq:\n  - id: data\n    size: encoded_size\n    process: zlib\n")
      schema("processed_rotate_child", "seq:\n  - id: child\n    size: 2\n    type: body\n    process: rol(1)\ntypes:\n  body:\n    seq:\n      - id: data\n        size-eos: true\n")

      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      val source = scala.io.Source.fromURL(getClass.getResource("/go-read-write/processed_check_test.go"))
      try write("processed_check_test.go", source.mkString) finally source.close()

      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-count=1", "-v", "./..."), dir.toFile).!(logger) == 0, s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
  }
}
