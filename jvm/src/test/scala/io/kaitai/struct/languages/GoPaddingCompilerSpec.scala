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

class GoPaddingCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  // Bounded reads always consume their complete region. These flags must not
  // change that, including when the terminator is absent or the region is empty.
  private case class Variant(repr: String, eos: Boolean, kind: String, include: Boolean, consume: Boolean, eosError: Boolean) {
    val id = s"padding_${repr}_${if (eos) "eos" else "size"}_${kind}_${include}_${consume}_${eosError}"
    val className = id.split("_").map(_.capitalize).mkString
    val bound = if (eos) "size-eos: true" else "size: width"
    val term = if (kind == "high") "255" else "46"
    val options = Seq(
      Some(bound),
      if (kind == "pad") None else Some(s"terminator: $term\n    include: $include\n    consume: $consume\n    eos-error: $eosError"),
      if (kind == "term") None else Some(s"pad-right: ${if (kind == "unequal" || kind == "pad") "43" else term}"),
      repr match { case "string" => Some("type: str\n    encoding: UTF-8"); case "child" => Some("type: child"); case _ => None }
    ).flatten.mkString("\n    ")
    val schema = s"""meta:
  id: $id
params:
  - id: width
    type: u4
seq:
${if (eos) "  - id: prefix\n    type: u1\n" else ""}  - id: value
    $options
""" + (if (repr == "child") "types:\n  child:\n    seq:\n      - id: data\n        size-eos: true\n" else "")

    def cases: String = kind match {
      case "equal" => if (include)
        """{"trailing", "ab...", "ab.", "ab..."}, {"all", ".....", ".", "....."}, {"interior", "a.xyz", "a.", "a...."}, {"last", "abcd.", "abcd.", "abcd."}, {"absent", "abcde", "abcde", "abcde"}, {"empty", "", "", ""},"""
      else
        """{"trailing", "ab...", "ab", "ab..."}, {"all", ".....", "", "....."}, {"interior", "a.xyz", "a", "a...."}, {"last", "abcd.", "abcd", "abcd."}, {"absent", "abcde", "abcde", "abcde"}, {"empty", "", "", ""},"""
      case "high" => if (include) """{"unsigned", "a\xff\xff", "a\xff", "a\xff\xff"},""" else """{"unsigned", "a\xff\xff", "a", "a\xff\xff"},"""
      case "unequal" => if (include && eos)
        """{"unequal", "ab.++", "ab.", "ab.++"}, {"short-no-term", "ab+++", "ab", "ab+++"}, {"empty", "+++++", "", "+++++"}, {"full-no-term", "abcde", "abcde", "abcde"},"""
      else if (include)
        """{"unequal", "ab.++", "ab.", "ab.++"},"""
      else
        """{"unequal", "ab.++", "ab", "ab.++"},"""
      case "pad" => """{"padding", "ab+++", "ab", "ab+++"},"""
      case "term" => if (include) """{"terminator", "ab...", "ab.", "ab.\x00\x00"},""" else """{"terminator", "ab...", "ab", "ab.\x00\x00"},"""
    }

    def goTest(rw: Boolean): String = {
      def value(obj: String): String = {
        val field = if (rw) s"$obj.GetValue()" else s"$obj.Value"
        repr match {
          case "child" => if (rw) s"$field.GetData()" else s"$field.Data"
          case _ => field
        }
      }
      val child = if (rw) "p.GetValue()" else "p.Value"
      val childCheck = if (repr == "child") s"""
        childPos, _ := $child.IO_().Pos()
        childSize, _ := $child.IO_().Size()
        if childPos != int64(len(tc.want)) || childSize != childPos { t.Fatalf("child position/size: %d/%d", childPos, childSize) }
""" else ""
      val write = if (!rw) "" else s"""
        if err := p.Check(); err != nil { t.Fatal(err) }
        output := kaitai.NewFixedSeekableBuffer(${if (eos) "len(input)" else "len(tc.input)"})
        if err := p.Write(kaitai.NewWriter(output)); err != nil { t.Fatal(err) }
        ${if (eos) "if output.Bytes()[0] != 0x7e || string(output.Bytes()[1:]) != tc.canonical { t.Fatalf(\"write %q want prefix+%q\", output.Bytes(), tc.canonical) }" else "if string(output.Bytes()) != tc.canonical { t.Fatalf(\"write %q want %q\", output.Bytes(), tc.canonical) }"}
        again := New$className(uint32(len(tc.input)))
        if err := again.Read(kaitai.NewStream(bytes.NewReader(output.Bytes())), nil, again); err != nil { t.Fatal(err) }
        if string(${value("again")}) != tc.want { t.Fatalf("reparse %q want %q", ${value("again")}, tc.want) }
"""
      val invalid = if (!rw || (!eos && kind != "equal")) "" else {
        val badValues = (kind, include) match {
          case ("pad", _) => s"{\"abcd+\", true}, {\"abcdef\", ${!eos}}"
          case ("high", true) => s"{\"ab\", true}, {\"a\\xffb\", true}, {\"abcdef\", ${!eos}}, {\"\", true}"
          case ("high", false) => s"{\"a\\xff\", true}, {\"ab\\xffcd\", true}, {\"abcdef\", ${!eos}}"
          case (_, true) if kind == "unequal" => s"{\"a.b\", true}, {\"abcd+\", true}, {\"abcdef\", ${!eos}}"
          case (_, true) => s"{\"ab\", true}, {\"a.b\", true}, {\"abcdef\", ${!eos}}, {\"\", true}"
          case ("unequal", false) => s"{\"a.\", true}, {\"ab.cd\", true}, {\"abcd+\", true}, {\"abcdef\", ${!eos}}"
          case (_, false) => s"{\"a.\", true}, {\"ab.cd\", true}, {\"abcdef\", ${!eos}}"
        }
        val setValue = repr match {
          case "string" => "p.SetValue(bad)"
          case "child" => "p.GetValue().SetData([]byte(bad))"
          case _ => "p.SetValue([]byte(bad))"
        }
        s"""
  for _, tc := range []struct { bad string; consistency bool }{$badValues} {
    p := New$className(5)
    input := []byte("abcde")
    ${if (eos) "input = append([]byte{0x7e}, input...)" else ""}
    if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil { t.Fatal(err) }
    bad := tc.bad
    $setValue
    var err error
    ${if (repr == "child") "err = p.GetValue().Check()" else ""}
    if err == nil { err = p.Check() }
    output := kaitai.NewFixedSeekableBuffer(${if (eos) "len(input)" else "5"})
    writer := kaitai.NewWriter(output)
    if err == nil { err = p.Write(writer) }
    if err == nil { t.Fatalf("accepted invalid value %q", bad) }
    ${if (eos) "var consistency kaitai.ConsistencyError\n    if errors.As(err, &consistency) != tc.consistency { t.Fatalf(\"invalid value %q consistency=%t: %v\", bad, tc.consistency, err) }" else ""}
    ${if (eos && repr != "child") "pos, posErr := writer.Pos()\n    if posErr != nil { t.Fatal(posErr) }\n    if pos != 1 { t.Fatalf(\"invalid value %q output position %d, want prefix position 1\", bad, pos) }" else ""}
  }
"""
      }
      s"""
func Test$className(t *testing.T) {
  for _, tc := range []struct { name, input, want, canonical string } { $cases } {
    t.Run(tc.name, func(t *testing.T) {
      p := New$className(uint32(len(tc.input)))
      input := []byte(tc.input)
      ${if (eos) "input = append([]byte{0x7e}, input...)" else "input = append(input, 0x7e)"}
      stream := kaitai.NewStream(bytes.NewReader(input))
      if err := p.Read(stream, nil, p); err != nil { t.Fatal(err) }
      if string(${value("p")}) != tc.want { t.Fatalf("read %q want %q", ${value("p")}, tc.want) }
      pos, _ := stream.Pos()
      if pos != int64(${if (eos) "len(input)" else "len(tc.input)"}) { t.Fatalf("outer position %d", pos) }
      $childCheck
      $write
    })
  }
  $invalid
}
"""
    }
  }

  private val variants = for {
    repr <- Seq("bytes", "string", "child")
    eos <- Seq(false, true)
    kind <- Seq("equal", "unequal", "pad", "term") ++ (if (repr != "string") Seq("high") else Nil)
    include <- if (kind == "pad") Seq(false) else Seq(false, true)
    flags <- if (kind == "equal") Seq((false, false), (false, true), (true, false), (true, true)) else Seq((true, true))
  } yield Variant(repr, eos, kind, include, flags._1, flags._2)

  for (readWrite <- Seq(false, true)) {
    sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
      test(s"bounded padding preserves included terminators in readWrite=$readWrite") {
        val dir = Files.createTempDirectory("kaitai-go-padding-")
        def write(name: String, text: String): Unit = { Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)); () }
        val runtime = new File(runtimePath).getCanonicalPath
        write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
        variants.foreach { variant =>
          write(s"${variant.id}.ksy", variant.schema)
          val config = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
          val (specsOption, problems) = JavaKSYParser.localFileToSpecs(dir.resolve(s"${variant.id}.ksy").toString, CLIConfig(runtime = config))
          val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
          Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach(f => write(new File(f.fileName).getName, f.contents))
        }
        val errorsImport = if (readWrite) "\"errors\"; " else ""
        write("padding_test.go", s"package contract\nimport (\"bytes\"; $errorsImport\"testing\"; \"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai\")\n" + variants.map(_.goTest(readWrite)).mkString)
        val output = new StringBuilder
        def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
        val logger = ProcessLogger(log, log)
        assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
        assert(Process(Seq("go", "test", "-mod=mod", "-v", "./..."), dir.toFile).!(logger) == 0, s"Generated Go in $dir failed:\n$output")
        info(s"Generated Go in $dir:\n$output")
      }
    }
  }
}
