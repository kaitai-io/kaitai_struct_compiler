package io.kaitai.struct.format

/**
  * Common abstract container for all identifiers that Kaitai Struct deals with.
  * Disables "toString" operation to make sure that identifier never goes into the
  * output stream without some sort of language-specific treatment (i.e. idToStr
  * or something like that)
  */
abstract class Identifier {
  override def toString: String = throw new UnsupportedOperationException
}

/**
  * Identifier generated automatically for seq attributes which lack true string "id" field.
  * @param idx unique number to identify attribute with
  */
case class NumberedIdentifier(idx: Int) extends Identifier

object NumberedIdentifier {
  val TEMPLATE = "unnamed"
}

/**
  * Named identifier for a seq attribute, parsed from "id" field.
  * @param name string to be used as identifier
  */
case class NamedIdentifier(name: String) extends Identifier {
  Identifier.checkIdentifier(name)
}

object Identifier {
  val ReIdentifier = "^[a-z][a-z0-9_]*$".r

  def checkIdentifier(id: String): Unit = {
    id match {
      case ReIdentifier() =>
      // name is valid, everything's fine
      case _ =>
        throw new RuntimeException("invalid identifier: \"" + id + "\"")
    }
  }
}

case class RawIdentifier(innerId: Identifier) extends Identifier

case class IoStorageIdentifier(innerId: Identifier) extends Identifier

case class InstanceIdentifier(name: String) extends Identifier {
  Identifier.checkIdentifier(name)
}

case class SpecialIdentifier(name: String) extends Identifier

object RootIdentifier extends SpecialIdentifier("_root")
object ParentIdentifier extends SpecialIdentifier("_parent")
object IoIdentifier extends SpecialIdentifier("_io")
