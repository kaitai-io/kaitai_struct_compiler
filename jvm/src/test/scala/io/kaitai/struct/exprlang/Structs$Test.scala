package io.kaitai.struct.exprlang

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.format._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class Structs$Test extends AnyFunSpec {
  describe("Structs.parse") {
    it("parses empty type") {
      val s = Structs.parse(
        """
          |type foo {
          |}
          |""".stripMargin
      )

      s.name should be(List("foo"))
    }

    it("parses simple integers seq with semicolons") {
      val s = Structs.parse(
        """
          |type foo {
          |  u4le hello;
          |  u1 world;
          |}
          |""".stripMargin
      )

      s.seq.length should be(2)

      s.seq(0).id should be(NamedIdentifier("hello"))
      s.seq(0).dataType should be(IntMultiType(signed = false, Width4, Some(LittleEndian)))

      s.seq(1).id should be(NamedIdentifier("world"))
      s.seq(1).dataType should be(Int1Type(signed = false))
    }

    it("parses simple integers seq without semicolons") {
      val s = Structs.parse(
        """
          |type foo {
          |  u4le hello
          |  u1 world
          |}
          |""".stripMargin
      )

      s.seq.length should be(2)

      s.seq(0).id should be(NamedIdentifier("hello"))
      s.seq(0).dataType should be(IntMultiType(signed = false, Width4, Some(LittleEndian)))

      s.seq(1).id should be(NamedIdentifier("world"))
      s.seq(1).dataType should be(Int1Type(signed = false))
    }

    it("parses seq + type in type") {
      val s = Structs.parse(
        """
          |type foo {
          |  u1 hello;
          |  type bar {
          |    u4le world;
          |  }
          |}
          |""".stripMargin
      )

      s.seq.length should be(1)

      s.seq(0).id should be(NamedIdentifier("hello"))
      s.seq(0).dataType should be(Int1Type(signed = false))

      s.types.size should be(1)
      val innerType = s.types("bar")
      innerType.seq.length should be(1)
      innerType.seq(0).id should be(NamedIdentifier("world"))
      innerType.seq(0).dataType should be(IntMultiType(signed = false, Width4, Some(LittleEndian)))
    }

    it("parses 2 types in type") {
      val s = Structs.parse(
        """
          |type foo {
          |  type bar {
          |    u4le world;
          |  }
          |  type baz {}
          |}
          |""".stripMargin
      )

      s.seq.length should be(0)

      s.types.size should be(2)
      val innerType1 = s.types("bar")
      innerType1.seq.length should be(1)
      innerType1.seq(0).id should be(NamedIdentifier("world"))
      innerType1.seq(0).dataType should be(IntMultiType(signed = false, Width4, Some(LittleEndian)))

      val innerType2 = s.types("baz")
      innerType2.seq.length should be(0)
    }

    it("parses user type seq") {
      val s = Structs.parse(
        """
          |type foo {
          |  animal dog;
          |  type animal {
          |    u2le code1;
          |    u2le code2;
          |  }
          |  animal cat;
          |}
          |""".stripMargin
      )

      s.seq.length should be(2)
      s.seq(0).id should be(NamedIdentifier("dog"))
      s.seq(0).dataType should be(UserTypeInstream(List("animal"), None, Seq()))
      s.seq(1).id should be(NamedIdentifier("cat"))
      s.seq(1).dataType should be(UserTypeInstream(List("animal"), None, Seq()))

      s.types.size should be(1)
      val innerType = s.types("animal")
      innerType.seq.length should be(2)
      innerType.seq(0).id should be(NamedIdentifier("code1"))
      innerType.seq(1).id should be(NamedIdentifier("code2"))
    }

    it("parses value instance with integer literal 42") {
      val s = Structs.parse(
        """
          |type foo {
          |  int_result = 42
          |}
          |""".stripMargin
      )

      s.seq.length should be(0)
      s.instances.size should be(1)

      val inst = s.instances(InstanceIdentifier("int_result"))
      inst.id should be(InstanceIdentifier("int_result"))
      inst match {
        case vis: ValueInstanceSpec =>
          vis.value should be(Ast.expr.IntNum(42))
      }
    }
  }
}
