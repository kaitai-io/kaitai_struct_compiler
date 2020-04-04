package io.kaitai.struct.translators

/**
  * Implementations of translations of literals in C-style, common to many
  * languages.
  */
trait CommonLiterals {
  def doIntLiteral(n: BigInt): String = n.toString
  def doFloatLiteral(n: Any): String = n.toString

  def doStringLiteral(s: String): String = {
    val encoded = s.toCharArray.map((code) =>
      if (code <= 0xff) {
        strLiteralAsciiChar(code)
      } else {
        strLiteralUnicode(code)
      }
    ).mkString
    "\"" + encoded + "\""
  }
  def doBoolLiteral(n: Boolean): String = n.toString

  /**
    * Handle ASCII character conversion for inlining into string literals.
    * Default implementation consults [[asciiCharQuoteMap]] first, then
    * just dumps it as is if it's a printable ASCII charcter, or calls
    * [[strLiteralGenericCC]] if it's a control character.
    * @param code character code to convert into string for inclusion in
    *             a string literal
    */
  def strLiteralAsciiChar(code: Char): String = {
    asciiCharQuoteMap.get(code) match {
      case Some(encoded) => encoded
      case None =>
        if (code >= 0x20 && code < 0x80) {
          Character.toString(code)
        } else {
          strLiteralGenericCC(code)
        }
    }
  }

  /**
    * Converts generic control character code into something that's allowed
    * inside a string literal. Default implementation uses octal encoding,
    * which is ok for most C-derived languages.
    *
    * Note that we use strictly 3 octal digits to work around potential
    * problems with following decimal digits, i.e. "\0" + "2" that would be
    * parsed as single character "\02" = "\x02", instead of two characters
    * "\x00\x32".
    * @param code character code to represent
    * @return string literal representation of given code
    */
  def strLiteralGenericCC(code: Char): String =
    "\\%03o".format(code.toInt)

  /**
    * Converts Unicode (typically, non-ASCII) character code into something
    * that's allowed inside a string literal. Default implementation uses
    * Unicode 4-digit hex encoding, which is ok for most C-derived languages.
    * @param code character code to represent
    * @return string literal representation of given code
    */
  def strLiteralUnicode(code: Char): String =
    "\\u%04x".format(code.toInt)

  /**
    * Character quotation map for inclusion in string literals.
    * Default implementation includes bare minimum that seems
    * to be available in all languages.
    */
  val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\"
  )

  def doRegexLiteral(reg: String): String = reg
  def doRegexFullLine(regex: String) : String = doRegexLiteral("""\A""" + regex + """\Z""")
}
