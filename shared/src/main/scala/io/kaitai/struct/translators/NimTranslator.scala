package io.kaitai.struct.translators

import io.kaitai.struct.languages.NimCompiler

class NimTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doIntLiteral(n: BigInt): String = {
    val literal = if (n > Long.MaxValue) {
          "0x" + n.toString(16)
        } else {
          n.toString
        }
        //val suffix = if (n > Int.MaxValue) "'u64" else ""

        s"$literal"//$suffix"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"@[${arr.mkString(", ")}]"
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"@[${elts.map(translate).mkString(", ")}]"

}
