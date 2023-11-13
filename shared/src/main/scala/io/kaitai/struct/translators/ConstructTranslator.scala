package io.kaitai.struct.translators

import io.kaitai.struct.ImportList
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.ConstructClassCompiler

class ConstructTranslator(provider: TypeProvider, importList: ImportList) extends PythonTranslator(provider, importList) {
  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "obj_"
      case Identifier.INDEX => "i"
      case Identifier.ROOT => "this._root"
      // In some cases the _io object is referenced within a lambda expression
      // and hasn't been defined before. It would automatically run into errors.
      // Using "this" as a local and global reference prevents this issue.
      case Identifier.IO => "this._io"
      case _ => s"this.${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case Identifier.PARENT => "_"
      case _ => s
    }
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"'$label'"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"stream_size(${translate(value)})"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"stream_iseof(${translate(value)})"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"stream_tell(${translate(value)})"
}
