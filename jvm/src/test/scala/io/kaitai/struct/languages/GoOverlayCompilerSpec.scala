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

class GoOverlayCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
    test("plain positioned user types overlay only bytes they write") {
      val dir = Files.createTempDirectory("kaitai-go-overlay-")
      def write(name: String, text: String): Unit = {
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
        ()
      }
      def schema(name: String, body: String): String = {
        val path = dir.resolve(s"$name.ksy")
        write(s"$name.ksy", s"meta:\n  id: $name\n$body")
        val config = RuntimeConfig(readWrite = true, goPackage = "contract")
        val (specsOption, problems) = JavaKSYParser.localFileToSpecs(path.toString, CLIConfig(runtime = config))
        val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
        val files = Main.compile(specs, specs.firstSpec, GoCompiler, config).files
        files.foreach(f => write(new File(f.fileName).getName, f.contents))
        files.map(_.contents).mkString("\n")
      }

      val empty = schema("overlay_empty", """seq:
  - id: data
    size: 6
  - id: tail
    type: u1
instances:
  view:
    pos: 1
    size: 4
    type: empty
types:
  empty: {}
""")
      val partial = schema("overlay_partial", """seq:
  - id: data
    size: 6
instances:
  view:
    pos: 1
    size: 4
    type: patch
types:
  patch:
    instances:
      first:
        pos: 0
        type: u1
      last:
        pos: 3
        type: u1
""")
      val eos = schema("overlay_eos", """seq:
  - id: data
    size: 6
instances:
  view:
    pos: 2
    size-eos: true
    type: empty
types:
  empty: {}
""")
      val external = schema("overlay_external", """params:
  - id: output
    type: io
seq:
  - id: marker
    type: u1
instances:
  view:
    io: output
    pos: 1
    size: 2
    type: empty
types:
  empty: {}
""")
      val processed = schema("overlay_processed", """seq:
  - id: data
    size: 4
instances:
  view:
    pos: 1
    size: 2
    process: xor(255)
    type: empty
types:
  empty: {}
""")
      val padded = schema("overlay_padded", """seq:
  - id: data
    size: 4
instances:
  view:
    pos: 1
    size: 2
    pad-right: 85
    type: empty
types:
  empty: {}
""")
      val terminated = schema("overlay_terminated", """seq:
  - id: data
    size: 4
instances:
  view:
    pos: 1
    size: 2
    terminator: 0
    type: empty
types:
  empty: {}
""")

      assert(empty.contains("NewOverlayChild"))
      assert(partial.contains("NewOverlayChild"))
      assert(eos.contains("NewOverlayChild"))
      assert(external.contains("NewOverlayChild"))
      for ((name, source) <- Seq("processed" -> processed, "padded" -> padded, "terminated" -> terminated)) {
        assert(!source.contains("NewOverlayChild"), s"$name child became sparse")
        assert(source.contains("NewChild"), s"$name child lost dense reservation")
      }

      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      val source = scala.io.Source.fromURL(getClass.getResource("/go-read-write/overlay_test.go"))
      try write("overlay_test.go", source.mkString) finally source.close()
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-count=1", "-v", "./..."), dir.toFile).!(logger) == 0, s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
  }
}
