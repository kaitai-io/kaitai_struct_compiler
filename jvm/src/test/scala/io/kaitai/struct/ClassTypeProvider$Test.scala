package io.kaitai.struct

import java.util.NoSuchElementException
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.formats.{JavaClassSpecs, JavaKSYParser}
import io.kaitai.struct.precompile.{EnumNotFoundError, MarkupClassNames, TypeNotFoundError}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ClassTypeProvider$Test extends AnyFunSpec {
  val root = ClassSpec.fromYaml(JavaKSYParser.stringToYaml("""
    meta:
      id: root
    enums:
      e: {} # e_root
    types:
      child_1:
        types:
          one: # child_11
            enums:
              e: {} # e_11
          two: # child_12
            enums:
              e: {} # e_12
            types:
              one: {} # child_121
              two: {} # child_122
      child_2:
        enums:
          e: {} # e_2
        types:
          one: {} # child_21
          two: {} # child_22
  """), None)
  val specs = new JavaClassSpecs("", Seq(), root)
  // Calculates full class names needed for work of the provider
  new MarkupClassNames(specs).run()

  val child_1 = root.types.get("child_1").getOrElse(throw new NoSuchElementException("'child_1' not found"))
  val child_2 = root.types.get("child_2").getOrElse(throw new NoSuchElementException("'child_2' not found"))

  val child_11 = child_1.types.get("one").getOrElse(throw new NoSuchElementException("'child_11' not found"))
  val child_12 = child_1.types.get("two").getOrElse(throw new NoSuchElementException("'child_12' not found"))

  val child_21 = child_2.types.get("one").getOrElse(throw new NoSuchElementException("'child_21' not found"))
  val child_22 = child_2.types.get("two").getOrElse(throw new NoSuchElementException("'child_22' not found"))

  val child_121 = child_12.types.get("one").getOrElse(throw new NoSuchElementException("'child_121' not found"))
  val child_122 = child_12.types.get("two").getOrElse(throw new NoSuchElementException("'child_122' not found"))

  describe("resolveTypeName") {
    describe("in 'root' context") {
      val resolver = new ClassTypeProvider(specs, root)

      it("resolves 'root'") {
        resolver.resolveTypeName(root, "root") should be(root) // self-reference
      }

      it("doesn't resolve 'one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(root, "one")
        thrown.getMessage should be("unable to find type 'one', searching from 'root'")
      }

      it("doesn't resolve 'two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(root, "two")
        thrown.getMessage should be("unable to find type 'two', searching from 'root'")
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(root, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root'")
      }
    }

    describe("in 'child_1' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_1 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_1, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_1, "one") should be(child_11)
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_1, "two") should be(child_12)
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_1, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_1'")
      }
    }

    describe("in 'child_2' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_2 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_2, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_2, "one") should be(child_21)
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_2, "two") should be(child_22)
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_2, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_2'")
      }
    }

    describe("in 'child_11' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_11 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_11, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_11, "one") should be(child_11) // self-reference
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_11, "two") should be(child_12)
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_11, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_1::one'")
      }
    }

    describe("in 'child_12' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_12 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_12, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_12, "one") should be(child_121)
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_12, "two") should be(child_12) // self-reference
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_12, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_1::two'")
      }
    }

    describe("in 'child_21' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_21 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_21, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_21, "one") should be(child_21) // self-reference
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_21, "two") should be(child_22)
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_21, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_2::one'")
      }
    }

    describe("in 'child_22' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_22 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_22, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_22, "one") should be(child_21)
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_22, "two") should be(child_22) // self-reference
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_22, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_2::two'")
      }
    }

    describe("in 'child_121' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_121 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_121, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_121, "one") should be(child_121) // self-reference
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_121, "two") should be(child_12)
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_121, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_1::two::one'")
      }
    }

    describe("in 'child_122' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_122 // Influences the error messages ("searching from '...'" part)

      it("resolves 'root'") {
        resolver.resolveTypeName(child_122, "root") should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypeName(child_122, "one") should be(child_121)
      }

      it("resolves 'two'") {
        resolver.resolveTypeName(child_122, "two") should be(child_122) // self-reference
      }

      it("doesn't resolve 'unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypeName(child_122, "unknown")
        thrown.getMessage should be("unable to find type 'unknown', searching from 'root::child_1::two::two'")
      }
    }
  }

  describe("resolveTypePath") {
    describe("in 'root' context") {
      val resolver = new ClassTypeProvider(specs, root)

      it("resolves empty path") {
        resolver.resolveTypePath(root, Seq()) should be(root) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(root, Seq("root")) should be(root) // self-reference
      }

      it("doesn't resolve 'one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("one"))
        thrown.getMessage should be("unable to find type 'one', searching from 'root'")
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'one', searching from 'root'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'one', searching from 'root'")
      }

      it("doesn't resolve 'two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("two"))
        thrown.getMessage should be("unable to find type 'two', searching from 'root'")
      }

      it("doesn't resolve 'two::one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("two", "one"))
        thrown.getMessage should be("unable to find type 'two', searching from 'root'")
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(root, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'two', searching from 'root'")
      }
    }

    describe("in 'child_1' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_1 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_1, Seq()) should be(child_1) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_1, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_1, Seq("one")) should be(child_11)
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_1, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_1::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_1, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_1, Seq("two")) should be(child_12)
      }

      it("resolves 'two::one'") {
        resolver.resolveTypePath(child_1, Seq("two", "one")) should be(child_121)
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_1, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two'")
      }
    }

    describe("in 'child_2' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_2 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_2, Seq()) should be(child_2) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_2, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_2, Seq("one")) should be(child_21)
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_2, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_2::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_2, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_2, Seq("two")) should be(child_22)
      }

      it("doesn't resolve 'two::one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_2, Seq("two", "one"))
        thrown.getMessage should be("unable to find type 'one' in 'root::child_2::two'")
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_2, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::two'")
      }
    }

    describe("in 'child_11' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_11 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_11, Seq()) should be(child_11) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_11, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_11, Seq("one")) should be(child_11) // self-reference
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_11, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_1::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_11, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_11, Seq("two")) should be(child_12)
      }

      it("resolves 'two::one'") {
        resolver.resolveTypePath(child_11, Seq("two", "one")) should be(child_121)
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_11, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two'")
      }
    }

    describe("in 'child_12' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_12 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_12, Seq()) should be(child_12) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_12, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_12, Seq("one")) should be(child_121)
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_12, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_1::two::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_12, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_12, Seq("two")) should be(child_12) // self-reference
      }

      it("resolves 'two::one'") {
        resolver.resolveTypePath(child_12, Seq("two", "one")) should be(child_121)
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_12, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two'")
      }
    }

    describe("in 'child_21' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_21 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_21, Seq()) should be(child_21) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_21, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_21, Seq("one")) should be(child_21) // self-reference
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_21, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_2::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_21, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_21, Seq("two")) should be(child_22)
      }

      it("doesn't resolve 'two::one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_21, Seq("two", "one"))
        thrown.getMessage should be("unable to find type 'one' in 'root::child_2::two'")
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_21, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::two'")
      }
    }

    describe("in 'child_22' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_22 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_22, Seq()) should be(child_22) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_22, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_22, Seq("one")) should be(child_21)
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_22, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_2::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_22, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_22, Seq("two")) should be(child_22) // self-reference
      }

      it("doesn't resolve 'two::one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_22, Seq("two", "one"))
        thrown.getMessage should be("unable to find type 'one' in 'root::child_2::two'")
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_22, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_2::two'")
      }
    }

    describe("in 'child_121' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_121 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_121, Seq()) should be(child_121) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_121, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_121, Seq("one")) should be(child_121) // self-reference
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_121, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_1::two::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_121, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_121, Seq("two")) should be(child_12)
      }

      it("doesn't resolve 'two::one'") {
        resolver.resolveTypePath(child_121, Seq("two", "one")) should be(child_121) // self-reference
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_121, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two'")
      }
    }

    describe("in 'child_122' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_122 // Influences the error messages ("searching from '...'" part)

      it("resolves empty path") {
        resolver.resolveTypePath(child_122, Seq()) should be(child_122) // self-reference
      }

      it("resolves 'root'") {
        resolver.resolveTypePath(child_122, Seq("root")) should be(root)
      }

      it("resolves 'one'") {
        resolver.resolveTypePath(child_122, Seq("one")) should be(child_121)
      }

      it("doesn't resolve 'one::two'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_122, Seq("one", "two"))
        thrown.getMessage should be("unable to find type 'two' in 'root::child_1::two::one'")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_122, Seq("one", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two::one'")
      }

      it("resolves 'two'") {
        resolver.resolveTypePath(child_122, Seq("two")) should be(child_122) // self-reference
      }

      it("doesn't resolve 'two::one'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_122, Seq("two", "one"))
        thrown.getMessage should be("unable to find type 'one' in 'root::child_1::two::two'")
      }

      it("doesn't resolve 'two::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveTypePath(child_122, Seq("two", "unknown"))
        thrown.getMessage should be("unable to find type 'unknown' in 'root::child_1::two::two'")
      }
    }
  }

  describe("resolveEnum") {
    val e_root = root.enums.get("e").getOrElse(throw new NoSuchElementException("'e_root' not found"))
    val e_11 = child_11.enums.get("e").getOrElse(throw new NoSuchElementException("'e_11' not found"))
    val e_12 = child_12.enums.get("e").getOrElse(throw new NoSuchElementException("'e_12' not found"))
    val e_2 = child_2.enums.get("e").getOrElse(throw new NoSuchElementException("'e_2' not found"))

    val none    = Ast.typeId(false, Seq())
    val one     = Ast.typeId(false, Seq("one"))
    val one_two = Ast.typeId(false, Seq("one", "two"))
    val unknown = Ast.typeId(false, Seq("unknown"))

    describe("in 'root' context") {
      val resolver = new ClassTypeProvider(specs, root)

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_root)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_1' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_1

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_root)
      }

      it("resolves 'one::e'") {
        resolver.resolveEnum(one, "e") should be(e_11)
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_2' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_2

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_2)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_11' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_11

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_11)
      }

      it("resolves 'one::e'") {
        resolver.resolveEnum(one, "e") should be(e_11)
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_12' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_12

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_12)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_21' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_21

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_2)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_22' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_22

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_2)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_121' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_121

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_12)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }

    describe("in 'child_122' context") {
      val resolver = new ClassTypeProvider(specs, root)
      resolver.nowClass = child_122

      it("resolves 'e'") {
        resolver.resolveEnum(none, "e") should be(e_12)
      }

      it("doesn't resolve 'one::e'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "e")
      }

      it("doesn't resolve 'one::two::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(one_two, "e")
      }

      it("doesn't resolve 'one::unknown'") {
        val thrown = the[EnumNotFoundError] thrownBy resolver.resolveEnum(one, "unknown")
      }

      it("doesn't resolve 'unknown::e'") {
        val thrown = the[TypeNotFoundError] thrownBy resolver.resolveEnum(unknown, "e")
      }
    }
  }
}
