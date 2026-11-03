package io.kaitai.struct.format

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.problems.CompilationProblemException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class BooleanIdentifiersSpec extends AnyFunSpec {
  def parse(yaml: String): ClassSpec =
    ClassSpec.fromYaml(JavaKSYParser.stringToYaml(yaml), None)

  val header = "meta:\n  id: boolean_identifiers\n"
  val locations: Seq[(String, String => String, List[String], ClassSpec => String)] = Seq(
    ("meta id", id => s"meta:\n  id: $id\n", List("meta", "id"), _.meta.id.get),
    ("sequence id", id => header + s"seq:\n  - id: $id\n    type: u1\n",
      List("seq", "0", "id"), _.seq.head.id.humanReadable),
    ("parameter id", id => header + s"params:\n  - id: $id\n    type: u1\n",
      List("params", "0", "id"), _.params.head.id.humanReadable),
    ("type name", id => header + s"types:\n  $id: {}\n",
      List("types"), _.types.head._1),
    ("instance name", id => header + s"instances:\n  $id:\n    value: 1\n",
      List("instances"), _.instances.head._1.humanReadable),
    ("enum name", id => header + s"enums:\n  $id:\n    0: enabled\n",
      List("enums"), _.enums.head._1),
    ("enum member name", id => header + s"enums:\n  flags:\n    0: $id\n",
      List("enums", "flags", "0"), _.enums("flags").map(BigInt(0)).name),
    ("enum member id", id => header + s"enums:\n  flags:\n    0:\n      id: $id\n",
      List("enums", "flags", "0", "id"), _.enums("flags").map(BigInt(0)).name)
  )
  val booleanWords = Seq("true", "false", "yes", "no", "on", "off")
  val booleanScalars = booleanWords.flatMap(word => Seq(word, word.capitalize, word.toUpperCase))

  locations.foreach { case (name, yaml, path, parsedName) =>
    describe(name) {
      booleanScalars.filter(_ => !name.startsWith("enum member")).foreach { scalar =>
        it(s"rejects unquoted $scalar at the source path") {
          val error = intercept[CompilationProblemException] { parse(yaml(scalar)) }
          error.problem.coords.path should be(Some(path))
          error.problem.text should include("expected string")
          error.problem.text should include("java.lang.Boolean")
        }
      }

      // Enum members deliberately accept booleans. At this layer, YAML aliases
      // such as `on` are indistinguishable from a literal `true`.
      if (name.startsWith("enum member")) {
        Seq("true", "false").flatMap(word => Seq(word, word.capitalize, word.toUpperCase)).foreach { scalar =>
          it(s"keeps unquoted $scalar as a boolean enum member") {
            parsedName(parse(yaml(scalar))) should be(scalar.toLowerCase)
          }
        }
      }

      booleanWords.foreach { word =>
        Seq("'", "\"").foreach { quote =>
          it(s"preserves $quote$word$quote as a string identifier") {
            parsedName(parse(yaml(quote + word + quote))) should be(word)
          }
        }
      }
    }
  }

  it("documents current enum on/off coercion as a known limitation") {
    // This records the current loader behavior, not a requirement to retain it.
    // Update these expectations when enum members preserve scalar spelling.
    val spec = parse(header + "enums:\n  flags:\n    0: on\n    1: off\n")
    spec.enums("flags").map(BigInt(0)).name should be("true")
    spec.enums("flags").map(BigInt(1)).name should be("false")
  }

  it("rejects the issue's on/off example instead of renaming the fields") {
    val error = intercept[CompilationProblemException] {
      parse("""meta:
              |  id: fileformat
              |  endian: le
              |seq:
              |  - id: on
              |    type: u4
              |  - id: off
              |    type: u4
              |""".stripMargin)
    }
    error.problem.coords.path should be(Some(List("seq", "0", "id")))
  }

  it("keeps booleans valid in expressions and metadata, and allows unnamed fields") {
    val spec = parse("""meta:
                       |  id: boolean_identifiers
                       |  ks-debug: true
                       |seq:
                       |  - type: u1
                       |    if: true
                       |instances:
                       |  enabled:
                       |    value: false
                       |""".stripMargin)
    spec.meta.forceDebug should be(true)
    spec.seq.head.id should be(NumberedIdentifier(0))
    spec.seq.head.cond.ifExpr should be(Some(Ast.expr.Bool(true)))
    spec.instances(InstanceIdentifier("enabled")).asInstanceOf[ValueInstanceSpec].value should be(Ast.expr.Bool(false))
  }
}
