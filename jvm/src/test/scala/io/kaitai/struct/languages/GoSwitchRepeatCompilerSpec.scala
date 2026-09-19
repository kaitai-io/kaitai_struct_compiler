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

class GoSwitchRepeatCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  // Changing an auxiliary append into a compact branch-only slice must break
  // these mixed typed/raw sequences, including when only one branch is processed.
  private case class Variant(repeat: String, kind: String) {
    val processed = kind == "processed"
    val id = s"switch_repeat_${repeat}_$kind"
    val className = id.split("_").map(_.capitalize).mkString
    val repeatOptions = repeat match {
      case "expr" => "repeat: expr\n    repeat-expr: count"
      case "until" => "repeat: until\n    repeat-until: '_io.eof and (codes[_index] == 1 ? _.as<one>.data.size == 2 : (codes[_index] == 2 ? _.as<two>.data.size == 2 : _.as<bytes>.size == 2))'"
      case "eos" => "repeat: eos"
    }
    val schema = s"""meta:
  id: $id
seq:
  - id: count
    type: u1
  - id: codes
    size: count
  - id: body
    size: 2
    ${if (kind == "padded") "pad-right: 0" else ""}
    ${if (processed) "process: xor(0xff)" else ""}
    $repeatOptions
    type:
      switch-on: codes[_index]
      cases:
        1: one
        2: two
types:
  one:
    seq:
      - id: data
        size-eos: true
  two:
    seq:
      - id: data
        size-eos: true
"""

    def goTest(rw: Boolean): String = {
      def field(o: String, f: String) = if (rw) s"$o.Get$f()" else s"$o.$f"
      val decode = if (processed) "want = []byte{want[0]^0xff,want[1]^0xff}" else ""
      val mutation = if (!rw) "" else s"""
      // Mutate the last typed value after any raw defaults, then require exact bytes.
      for i := len(codes)-1; i >= 0; i-- {
        switch child := p.GetBody()[i].(type) {
        case *${className}_One: child.SetData([]byte{0x34,0x56}); if err := child.Check(); err != nil { t.Fatal(err) }
        case *${className}_Two: child.SetData([]byte{0x34,0x56}); if err := child.Check(); err != nil { t.Fatal(err) }
        default: continue
        }
        want[1+len(codes)+2*i] = ${if (processed) "0xcb" else "0x34"}
        want[2+len(codes)+2*i] = ${if (processed) "0xa9" else "0x56"}
        break
      }
      if err := p.Check(); err != nil { t.Fatal(err) }
      output := kaitai.NewFixedSeekableBuffer(len(input))
      if err := p.Write(kaitai.NewWriter(output)); err != nil { t.Fatal(err) }
      if !bytes.Equal(output.Bytes(),want) { t.Fatalf("mutation bytes %x want %x",output.Bytes(),want) }
      again := New$className()
      if err := again.Read(kaitai.NewStream(bytes.NewReader(output.Bytes())),nil,again); err != nil { t.Fatal(err) }
      if err := again.Check(); err != nil { t.Fatal(err) }
      second := kaitai.NewFixedSeekableBuffer(len(input))
      if err := again.Write(kaitai.NewWriter(second)); err != nil { t.Fatal(err) }
      if !bytes.Equal(second.Bytes(),want) { t.Fatalf("reparse bytes %x want %x",second.Bytes(),want) }
"""
      val write = if (!rw) "" else s"""
      if err := p.Check(); err != nil { t.Fatal(err) }
      output := kaitai.NewFixedSeekableBuffer(len(input))
      if err := p.Write(kaitai.NewWriter(output)); err != nil { t.Fatal(err) }
      if !bytes.Equal(output.Bytes(),input) { t.Fatalf("roundtrip %x want %x",output.Bytes(),input) }
      want := bytes.Clone(input)
      { $mutation }
"""
      s"""
func Test$className(t *testing.T) {
  for name,codes := range map[string][]byte {
    "middle": {1,7,2}, "leading": {7,1,2}, "trailing": {1,2,7},
    "consecutive": {1,7,8,2}, "all_default": {7,8,9}, "all_typed": {1,2,1},
  } {
    t.Run(name,func(t *testing.T) {
      input := append([]byte{byte(len(codes))},codes...)
      for i := range codes { input = append(input,byte(0x10+i),byte(0x20+i)) }
      p := New$className()
      if err := p.Read(kaitai.NewStream(bytes.NewReader(input)),nil,p); err != nil { t.Fatal(err) }
      if len(${field("p", "Body")}) != len(codes) { t.Fatalf("logical body count %d want %d",len(${field("p", "Body")}),len(codes)) }
      for i,code := range codes {
        want := []byte{byte(0x10+i),byte(0x20+i)}
        $decode
        var got []byte
        switch code {
        case 1: child,ok := ${field("p", "Body")}[i].(*${className}_One); if !ok { t.Fatalf("entry %d expected one",i) }; got = ${field("child", "Data")}
        case 2: child,ok := ${field("p", "Body")}[i].(*${className}_Two); if !ok { t.Fatalf("entry %d expected two",i) }; got = ${field("child", "Data")}
        default: var ok bool; got,ok = ${field("p", "Body")}[i].([]byte); if !ok { t.Fatalf("entry %d expected bytes",i) }
        }
        if !bytes.Equal(got,want) { t.Fatalf("entry %d got %x want %x",i,got,want) }
      }
      $write
    })
  }
}
"""
    }
  }

  for (readWrite <- Seq(false, true); repeat <- Seq("expr", "until", "eos")) {
    sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
      test(s"repeated switches preserve logical raw indices with $repeat in readWrite=$readWrite") {
        val dir = Files.createTempDirectory("kaitai-go-switch-repeat-")
        def write(name: String, text: String): Unit = { Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)); () }
        val runtime = new File(runtimePath).getCanonicalPath
        write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
        val variants = Seq("plain", "processed", "padded").map(Variant(repeat, _))
        variants.foreach { variant =>
          write(s"${variant.id}.ksy", variant.schema)
          val config = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
          val (specsOption, problems) = JavaKSYParser.localFileToSpecs(dir.resolve(s"${variant.id}.ksy").toString, CLIConfig(runtime = config))
          val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
          Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach(f => write(new File(f.fileName).getName, f.contents))
        }
        write("switch_repeat_test.go", "package contract\nimport (\"bytes\"; \"testing\"; \"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai\")\n" + variants.map(_.goTest(readWrite)).mkString)
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
