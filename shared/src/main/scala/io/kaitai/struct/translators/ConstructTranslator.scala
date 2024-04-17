package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, RuntimeConfig}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.ConstructClassCompiler

class ConstructTranslator(provider: TypeProvider, importList: ImportList) extends PythonTranslator(provider, importList, RuntimeConfig()) {
  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "obj_"
      case Identifier.INDEX => "i"
      case Identifier.ROOT => "this._root"
      case Identifier.IO => "_io"
      case _ => s"this.${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case Identifier.PARENT => "_"
      case _ => s
    }
  }

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String =
    s"'$label'"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"stream_size(${translate(value)})"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"stream_iseof(${translate(value)})"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"stream_tell(${translate(value)})"
}
