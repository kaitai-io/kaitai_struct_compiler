package io.kaitai.struct.format

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.problems.KSYParseError

/**
  * Common abstract container for all identifiers that Kaitai Struct deals with.
  */
abstract class Identifier {
  /**
    * @return Human-readable name of identifier, to be used exclusively for ksc
    *         error messaging purposes.
    */
  def humanReadable: String
}

/**
  * Identifier generated automatically for seq attributes which lack true string "id" field.
  * @param idx unique number to identify attribute with
  */
case class NumberedIdentifier(idx: Int) extends Identifier {
  import NumberedIdentifier._
  override def humanReadable: String = s"${TEMPLATE}_$idx"
}

object NumberedIdentifier {
  val TEMPLATE = "unnamed"
}

/**
  * Named identifier for a seq attribute, parsed from "id" field.
  * @param name string to be used as identifier
  */
case class NamedIdentifier(name: String) extends Identifier {
  Identifier.checkIdentifier(name)

  override def humanReadable: String = name
}

case class InvalidIdentifier(id: String) extends RuntimeException(
  s"invalid ID: '$id', expected /${Identifier.ReIdentifier.toString}/"
)

object Identifier {
  val ReIdentifier = "^[a-z][a-z0-9_]*$".r

  def checkIdentifier(id: String): Unit = {
    id match {
      case ReIdentifier() =>
        // name is valid, everything's fine
      case _ =>
        throw new InvalidIdentifier(id)
    }
  }

  /**
    * Check if a given string is a valid identifier. If not, throw a custom
    * YAMLParseException, properly annotated with entity info and source file
    * path.
    * @param id string to check as identifier
    * @param entity which entity this object represents (i.e. "instance", "enum", etc)
    * @param path path in a source .ksy file to report in exception
    */
  def checkIdentifierSource(id: String, entity: String, path: List[String]): Unit = {
    id match {
      case ReIdentifier() =>
        // name is valid, everything's fine
      case _ =>
        throw KSYParseError.invalidId(id, entity, path)
    }
  }

  // Constants for special names used in expression language
  val ROOT = "_root"
  val PARENT = "_parent"
  val IO = "_io"
  val ITERATOR = "_"
  val ITERATOR2 = "_buf"
  val INDEX = "_index"
  val SWITCH_ON = "_on"
  val IS_LE = "_is_le"
  val SIZEOF = "_sizeof"
}

case class RawIdentifier(innerId: Identifier) extends Identifier {
  override def humanReadable: String = s"raw(${innerId.humanReadable})"
}

case class IoStorageIdentifier(innerId: Identifier) extends Identifier {
  override def humanReadable: String = s"io(${innerId.humanReadable})"
}

case class InstanceIdentifier(name: String) extends Identifier {
  Identifier.checkIdentifier(name)

  override def humanReadable: String = name
}

case class SpecialIdentifier(name: String) extends Identifier {
  override def humanReadable: String = name
}

object RootIdentifier extends SpecialIdentifier(Identifier.ROOT)
object ParentIdentifier extends SpecialIdentifier(Identifier.PARENT)
object IoIdentifier extends SpecialIdentifier(Identifier.IO)
object EndianIdentifier extends SpecialIdentifier(Identifier.IS_LE)
