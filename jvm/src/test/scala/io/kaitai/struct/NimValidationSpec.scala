package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, KSVersion}
import io.kaitai.struct.formats.{JavaClassSpecs, JavaKSYParser}
import io.kaitai.struct.languages.NimCompiler
import org.scalatest.funsuite.AnyFunSuite

class NimValidationSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  private def compile(body: String): String = {
    val spec = ClassSpec.fromYaml(
      JavaKSYParser.stringToYaml("meta:\n  id: validation_probe\n" + body), None)
    val specs = new JavaClassSpecs(".", Seq.empty, spec)
    val problems = Main.precompile(specs, RuntimeConfig())
    assert(problems.isEmpty, problems.mkString("\n"))
    Main.compile(specs, specs.firstSpec, NimCompiler, RuntimeConfig()).files.head.contents
  }

  test("equality validation emits an exception instead of being omitted") {
    val code = compile("seq:\n  - id: value\n    type: u1\n    valid: 1\n")
    assert(code.contains("if not (this.value == 1):"))
    assert(code.contains("raise newException(KaitaiError,"))
  }

  test("range validation emits both bounds") {
    val code = compile("seq:\n  - id: value\n    type: u1\n    valid:\n      min: 2\n      max: 4\n")
    assert(code.contains("if not (this.value >= 2):"))
    assert(code.contains("if not (this.value <= 4):"))
  }

  test("validation expressions bind the current value in a local scope") {
    val code = compile("seq:\n  - id: value\n    type: u1\n    valid:\n      expr: _ > 2\n")
    assert(code.contains("block:\n    let it = this.value"))
    assert(code.contains("if not (it > 2):"))
  }

  test("repeated validation uses the current element") {
    val code = compile("seq:\n  - id: values\n    type: u1\n    valid:\n      expr: _ > 2\n    repeat: expr\n    repeat-expr: 2\n")
    assert(code.contains("let it = this.values[i]"))
    assert(code.contains("if not (it > 2):"))
  }

  test("lazy instance validation uses cached storage without recursive access") {
    val code = compile("instances:\n  token:\n    pos: 0\n    type: u1\n    valid: 9\n")
    assert(code.contains("if not (this.tokenInst == 9):"))
  }

  test("lazy instance validation expressions use cached storage for self-references") {
    val code = compile("instances:\n  token:\n    pos: 0\n    type: u1\n    valid:\n      expr: token == 9\n")
    assert(code.contains("if not (this.tokenInst == 9):"))
    assert(!code.contains("if not (this.token == 9):"))
  }

  test("cached self-reference translation is limited to the instance being validated") {
    val code = compile("instances:\n  token:\n    pos: 0\n    type: u1\n    valid:\n      expr: token == other\n  other:\n    pos: 1\n    type: u1\n    valid: 9\n  copy:\n    value: token\n")
    assert(code.contains("if not (this.tokenInst == this.other):"))
    assert(code.contains("let copyInstExpr = uint8(this.token)"))
  }

  test("fixed contents and any-of validation are emitted") {
    val code = compile("seq:\n  - id: magic\n    contents: [0x4f, 0x4b]\n  - id: value\n    type: u1\n    valid:\n      any-of: [5, 7]\n")
    assert(code.contains("if not (this.magic == @[79'u8, 75'u8]):"))
    assert(code.contains("this.value == 5"))
    assert(code.contains("this.value == 7"))
  }
}
