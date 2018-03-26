package io.kaitai.struct.translators

import io.kaitai.struct.ImportList
import io.kaitai.struct.format.Identifier

class ConstructTranslator(provider: TypeProvider, importList: ImportList) extends PythonTranslator(provider, importList) {
  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "obj_"
      case Identifier.INDEX => "i"
      case Identifier.ROOT => "_root"
      case Identifier.IO => "_stream"
      case _ => s"this.${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case Identifier.PARENT => "_"
      case _ => s
    }
  }
}
