package io.kaitai.struct.translators

import io.kaitai.struct.languages.NimCompiler

class NimTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}.${translate(idx)}"

  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(if ${translate(condition)}: ${translate(ifTrue)} else: ${translate(ifFalse)})"

  override def doCast(value: Ast.expr, typeName: DataType): String =
    s"${NimCompiler.kaitaiType2NimType(typeName)}(${translate(value)})"

  override def strToInt(s: expr, base: expr): String =
    s"parseInt(${translate(s)}, ${translate(base)}"

  override def doIntLiteral(n: BigInt): String = {
    val literal = if (n > Long.MaxValue) {
          "0x" + n.toString(16)
        } else {
          n.toString
        }
        //val suffix = if (n > Int.MaxValue) "'u64" else ""

        s"$literal"//$suffix"
  }

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String =
    s"@[${value.map((v) => translate(v)).mkString(", ")}]"
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"@[${arr.mkString(", ")}]"
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"@[${elts.map(translate).mkString(", ")}]"


}
