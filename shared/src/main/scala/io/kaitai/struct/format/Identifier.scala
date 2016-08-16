package io.kaitai.struct.format

abstract class Identifier {
  override def toString: String = throw new UnsupportedOperationException
}

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
