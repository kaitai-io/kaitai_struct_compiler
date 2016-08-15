package io.kaitai.struct.format

abstract class Identifier

case class NamedIdentifier(name: String) extends Identifier {
  name match {
    case Identifier.ReIdentifier(_) =>
      // name is valid, everything's fine
    case _ =>
      throw new RuntimeException("invalid identifier: \"" + name + "\"")
  }
}

object Identifier {
  val ReIdentifier = "^[a-z][a-z0-9_]*$".r
}

case class RawIdentifier(innerId: Identifier) extends Identifier

case class InstanceIdentifier(name: String) extends Identifier {
  name match {
    case Identifier.ReIdentifier(_) =>
      // name is valid, everything's fine
    case _ =>
      throw new RuntimeException("invalid identifier: \"" + name + "\"")
  }
}

class SpecialIdentifier(name: String) extends Identifier

case object RootIdentifier extends SpecialIdentifier("_root")
case object ParentIdentifier extends SpecialIdentifier("_parent")
case object IoIdentifier extends SpecialIdentifier("_io")
