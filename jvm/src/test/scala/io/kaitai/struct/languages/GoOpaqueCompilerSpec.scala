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

class GoOpaqueCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  private val paramsDef = """meta:
  id: params_def
  endian: le
params:
  - id: len
    type: u4
  - id: has_trailer
    type: bool
seq:
  - id: buf
    type: str
    size: len
    encoding: UTF-8
  - id: trailer
    type: u1
    if: has_trailer
"""
  private val externalChild = """meta:
  id: opaque_external_type_02_child
  endian: le
seq:
  - id: s1
    type: str
    encoding: UTF-8
    terminator: 0x7c
  - id: s2
    type: str
    encoding: UTF-8
    terminator: 0x7c
    consume: false
  - id: s3
    type: opaque_external_type_02_child_child
types:
  opaque_external_type_02_child_child:
    seq:
      - id: s3
        type: str
        encoding: UTF-8
        terminator: 0x40
        include: true
        if: _root.some_method
instances:
  some_method:
    value: true
"""
  private val externalParent = """meta:
  id: opaque_external_type_02_parent
  ks-opaque-types: true
seq:
  - id: parent
    type: parent_obj
types:
  parent_obj:
    seq:
      - id: child
        type: opaque_external_type_02_child
"""

  // Dropping opaque arguments breaks the constructor ABI; using the caller's
  // root (or nil) breaks root-dependent fields in a separately generated type.
  for (rw <- Seq(false, true); contract <- Seq("params", "root")) {
    sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
      test(s"opaque $contract contract executes in readWrite=$rw") {
        val dir = Files.createTempDirectory("kaitai-go-opaque-")
        def write(name: String, text: String): Unit = { Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)); () }
        def field(owner: String, name: String): String = if (rw) s"$owner.Get$name()" else s"$owner.$name"
        val schemas = if (contract == "params") Seq(
          "params_def" -> paramsDef,
          "opaque_with_param" -> "meta:\n  id: opaque_with_param\n  ks-opaque-types: true\nseq:\n  - id: one\n    type: params_def(5, true)\n",
          "opaque_expr" -> """meta:
  id: opaque_expr
  ks-opaque-types: true
  endian: le
seq:
  - id: len
    type: u4
  - id: one
    type: params_def((alias + 1).as<u4>, len > 0)
instances:
  alias:
    value: len
""",
          "opaque_alias" -> "meta:\n  id: opaque_alias\n  ks-opaque-types: true\nparams:\n  - id: length\n    type: u4\nseq:\n  - id: one\n    type: params_def(alias, true)\ninstances:\n  alias:\n    value: length\n",
          "internal_params" -> "meta:\n  id: internal_params\nseq:\n  - id: length\n    type: u1\n  - id: one\n    type: child(length)\ntypes:\n  child:\n    params:\n      - id: length\n        type: u4\n    seq:\n      - id: data\n        size: length\n",
          "imported_params" -> "meta:\n  id: imported_params\n  imports: [params_def]\nseq:\n  - id: one\n    type: params_def(5, true)\n"
        ) else Seq("opaque_external_type_02_child" -> externalChild, "opaque_external_type_02_parent" -> externalParent)
        schemas.foreach { case (name, schema) => write(s"$name.ksy", schema) }
        schemas.foreach { case (name, _) =>
          val config = RuntimeConfig(readWrite = rw, goPackage = "contract")
          val (specsOption, problems) = JavaKSYParser.localFileToSpecs(dir.resolve(s"$name.ksy").toString, CLIConfig(runtime = config))
          val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
          Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach(f => write(new File(f.fileName).getName, f.contents))
        }
        val runtime = new File(runtimePath).getCanonicalPath
        write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
        val body = if (contract == "params") s"""
func TestOpaqueParams(t *testing.T) {
  input := []byte("foo|ba")
  p := NewOpaqueWithParam()
  stream := kaitai.NewStream(bytes.NewReader(input))
  if err := p.Read(stream,nil,p); err != nil { t.Fatal(err) }
  c := ${field("p", "One")}
  if ${field("c", "Buf")} != "foo|b" || ${field("c", "Trailer")} != 0x61 { t.Fatal("literal arguments lost") }
  if pos,err := stream.Pos(); err != nil || pos != 6 { t.Fatalf("position %d %v",pos,err) }
  e := NewOpaqueExpr()
  if err := e.Read(kaitai.NewStream(bytes.NewReader(append([]byte{4,0,0,0},input...))),nil,e); err != nil { t.Fatal(err) }
  ec := ${field("e", "One")}
  if ${field("ec", "Buf")} != "foo|b" || ${field("ec", "Trailer")} != 0x61 { t.Fatal("expression arguments lost") }
  a := NewOpaqueAlias(5)
  if err := a.Read(kaitai.NewStream(bytes.NewReader(input)),nil,a); err != nil { t.Fatal(err) }
  ac := ${field("a", "One")}
  if ${field("ac", "Buf")} != "foo|b" || ${field("ac", "Trailer")} != 0x61 { t.Fatal("typed alias argument lost") }
  imported := NewImportedParams()
  if err := imported.Read(kaitai.NewStream(bytes.NewReader(input)),nil,imported); err != nil { t.Fatal(err) }
  ic := ${field("imported", "One")}
  if ${field("ic", "Buf")} != "foo|b" { t.Fatal("typed imported argument lost") }
  if ic._root != ic || ic._parent != nil { t.Fatal("imported root/parent identity") }
  internal := NewInternalParams()
  if err := internal.Read(kaitai.NewStream(bytes.NewReader([]byte{2,0x61,0x62})),nil,internal); err != nil { t.Fatal(err) }
  inner := ${field("internal", "One")}
  if !bytes.Equal(${field("inner", "Data")},[]byte("ab")) || inner._root != internal || inner._parent != internal { t.Fatal("typed internal constructor/root") }
  short := NewOpaqueWithParam()
  if err := short.Read(kaitai.NewStream(bytes.NewReader(input[:2])),nil,short); err == nil { t.Fatal("short input accepted") }
  ${if (!rw) "" else """
  if err := e.Check(); err != nil { t.Fatal(err) }
  exprOut := kaitai.NewFixedSeekableBuffer(10)
  if err := e.Write(kaitai.NewWriter(exprOut)); err != nil { t.Fatal(err) }
  if !bytes.Equal(exprOut.Bytes(),[]byte{4,0,0,0,'f','o','o','|','b','a'}) { t.Fatalf("expression output %x",exprOut.Bytes()) }
  for _, object := range []interface{Check() error; Write(*kaitai.Writer) error}{p,imported,a} {
    if err := object.Check(); err != nil { t.Fatal(err) }
    out := kaitai.NewFixedSeekableBuffer(6)
    if err := object.Write(kaitai.NewWriter(out)); err != nil { t.Fatal(err) }
    if !bytes.Equal(out.Bytes(),input) { t.Fatalf("output %q",out.Bytes()) }
    again := NewOpaqueWithParam()
    if err := again.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())),nil,again); err != nil { t.Fatal(err) }
    if again.GetOne().GetBuf() != "foo|b" || again.GetOne().GetTrailer() != 0x61 { t.Fatal("reparse") }
  }
  fresh := NewOpaqueWithParam(); fresh.SetRoot_(fresh)
  child := NewParamsDef(5,true); child.SetRoot_(child)
  child.SetBuf("foo|b"); child.SetTrailer(0x61); fresh.SetOne(child)
  if err := child.Check(); err != nil { t.Fatal(err) }; if err := fresh.Check(); err != nil { t.Fatal(err) }
  out := kaitai.NewFixedSeekableBuffer(6)
  if err := fresh.Write(kaitai.NewWriter(out)); err != nil { t.Fatal(err) }
  if !bytes.Equal(out.Bytes(),input) { t.Fatalf("fresh %q",out.Bytes()) }
  child.SetBuf("bad"); if err := child.Check(); err == nil { t.Fatal("wrong length accepted") }
"""}
}
""" else s"""
func TestOpaqueRoot(t *testing.T) {
  input := []byte("foo|bar|baz@unconsumed")
  p := NewOpaqueExternalType02Parent()
  stream := kaitai.NewStream(bytes.NewReader(input))
  if err := p.Read(stream,nil,p); err != nil { t.Fatal(err) }
  parent := ${field("p", "Parent")}; child := ${field("parent", "Child")}; nested := ${field("child", "S3")}
  if ${field("child", "S1")} != "foo" || ${field("child", "S2")} != "bar" || ${field("nested", "S3")} != "|baz@" { t.Fatal("external root fields") }
  if child._root != child || nested._root != child || child._parent != nil || nested._parent != child { t.Fatal("external root/parent identity") }
  if pos,err := stream.Pos(); err != nil || pos != 12 { t.Fatalf("consumed %d %v",pos,err) }
  ${if (!rw) "" else """
  fresh := NewOpaqueExternalType02Parent(); fresh.SetRoot_(fresh)
  fp := NewOpaqueExternalType02Parent_ParentObj(); fp.SetParent_(fresh); fp.SetRoot_(fresh); fresh.SetParent(fp)
  fc := NewOpaqueExternalType02Child(); fc.SetRoot_(fc); fc.SetS1("foo"); fc.SetS2("bar"); fp.SetChild(fc)
  fn := NewOpaqueExternalType02Child_OpaqueExternalType02ChildChild(); fn.SetParent_(fc); fn.SetRoot_(fc); fn.SetS3("|baz@"); fc.SetS3(fn)
  for _, object := range []interface{Check() error}{fn,fc,fp,fresh} { if err := object.Check(); err != nil { t.Fatal(err) } }
  for _, object := range []*OpaqueExternalType02Parent{p,fresh} {
    if err := object.Check(); err != nil { t.Fatal(err) }
    out := kaitai.NewFixedSeekableBuffer(12)
    if err := object.Write(kaitai.NewWriter(out)); err != nil { t.Fatal(err) }
    if !bytes.Equal(out.Bytes(),[]byte("foo|bar|baz@")) { t.Fatalf("prefix output %q",out.Bytes()) }
    again := NewOpaqueExternalType02Parent()
    if err := again.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())),nil,again); err != nil { t.Fatal(err) }
    if again.GetParent().GetChild().GetS3().GetS3() != "|baz@" { t.Fatal("reparsed nested root") }
  }
"""}
}
"""
        write("opaque_test.go", "package contract\nimport (\"bytes\"; \"testing\"; \"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai\")\n" + body)
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
