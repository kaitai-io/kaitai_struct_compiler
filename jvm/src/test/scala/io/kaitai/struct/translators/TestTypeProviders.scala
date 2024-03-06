package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{CalcIntType, CalcStrType, CalcUserType, UserTypeInstream}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, FixedSized, Identifier}

/**
 * A few implementations of [[TypeProvider]] for test purposes.
 */
object TestTypeProviders {
  /**
   * Common type provider implementation for test purposes.
   */
  abstract class FakeTypeProvider extends TypeProvider {
    val nowClass = ClassSpec.opaquePlaceholder(List("top_class"))

    override def resolveEnum(inType: Ast.typeId, enumName: String) =
      throw new NotImplementedError

    override def resolveType(typeName: Ast.typeId): DataType = {
      if (typeName == Ast.typeId(false, List("block"), false)) {
        val name = List("top_class", "block")
        val r = CalcUserType(name, None, Seq())
        val cs = ClassSpec.opaquePlaceholder(name)
        cs.seqSize = FixedSized(56)
        r.classSpec = Some(cs)
        return r
      } else {
        throw new NotImplementedError
      }
    }

    override def isLazy(attrName: String): Boolean = false

    override def isLazy(inClass: ClassSpec, attrName: String): Boolean = false
  }

  /**
   * Type provider that always return one data type when question arises.
   * @param t data type to return
   */
  case class Always(t: DataType) extends FakeTypeProvider {
    override def determineType(name: String): DataType = t

    override def determineType(id: Identifier): DataType = t

    override def determineType(inClass: ClassSpec, name: String): DataType = t

    override def determineType(inClass: ClassSpec, id: Identifier): DataType = t
  }

  /**
   * Emulates the following system of types:
   *
   * {{{
   * meta:
   *   id: top_class
   * types:
   *   block:
   *     seq:
   *       - id: bar
   *         type: str
   *       - id: inner
   *         type: innerblock
   *     types:
   *       innerblock:
   *         instances:
   *           baz:
   *             value: 123
   * }}}
   */
  case object FooBarProvider extends FakeTypeProvider {
    override def determineType(name: String): DataType = {
      name match {
        case "foo" => userOwnedType(List("top_class", "block"))
      }
    }

    override def determineType(id: Identifier): DataType = ???

    override def determineType(inClass: ClassSpec, name: String): DataType = {
      (inClass.name.last, name) match {
        case ("block", "bar") => CalcStrType
        case ("block", "inner") => userOwnedType(List("top_class", "block", "innerblock"))
        case ("innerblock", "baz") => CalcIntType
      }
    }

    override def determineType(inClass: ClassSpec, id: Identifier): DataType = ???

    override def resolveType(typeName: Ast.typeId): DataType = {
      typeName.names match {
        case Seq("top_class") =>
          userOwnedType(List("top_class"))
        case Seq("block") |
             Seq("top_class", "block") =>
          userOwnedType(List("top_class", "block"))
        case Seq("innerblock") |
             Seq("block", "innerblock") |
             Seq("top_class", "block", "innerblock") =>
          userOwnedType(List("top_class", "block", "innerblock"))
      }
    }
  }

  def userOwnedType(lname: List[String]): UserTypeInstream = {
    val cs = ClassSpec.opaquePlaceholder(lname)
    val ut = UserTypeInstream(lname, None)
    ut.classSpec = Some(cs)
    ut
  }

  def userBorrowedType(lname: List[String]): CalcUserType = {
    val cs = ClassSpec.opaquePlaceholder(lname)
    val ut = CalcUserType(lname, None)
    ut.classSpec = Some(cs)
    ut
  }
}
