package io.kaitai.struct.translators

import io.kaitai.struct.ImportList
import io.kaitai.struct.format.Identifier

class ConstructTranslator(provider: TypeProvider, importList: ImportList) extends PythonTranslator(provider, importList) {
  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "_"
      case Identifier.INDEX => "i"
      case _ => s"this.${doName(s)}"
    }
  }
}
