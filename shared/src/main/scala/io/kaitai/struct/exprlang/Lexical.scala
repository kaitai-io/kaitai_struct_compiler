package io.kaitai.struct.exprlang

object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)

/**
 * Loosely based on /pythonparse/shared/src/main/scala/pythonparse/
 * from FastParse, Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)
 * http://www.lihaoyi.com/fastparse/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and
 * to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 * THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
object Lexical {
  import fastparse.all._
  def kw(s: String) = s ~ !namePart

  val wscomment = P( (CharsWhile(" \n".toSet, min = 1) | "\\\n").rep )

  val nameStart = P( letter | "_" )
  val namePart  = P( letter | digit | "_" )
  val identifier: P[Ast.identifier] =
    P( nameStart ~ namePart.rep ).!.map(Ast.identifier)
  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  val stringliteral: P[String] = P( singlestring | doublestring )
  val singlestring = P("'" ~/ singlestringchar.rep.! ~ "'")
  val singlestringchar = P( CharsWhile(!"'".contains(_)) )

  val doublestring: P[String] = P("\"" ~/ doublestringitem.rep ~ "\"").map(_.mkString)
  val doublestringitem = P( doublestringchar.! | escapeseq )
  val doublestringchar = P( CharsWhile(!"\\\"".contains(_)) )
  val escapeseq = P( "\\" ~/ (quotedchar | quotedoctal | quotedhex) )

  val QUOTED_CC = Map(
    "a" -> "\u0007", // bell, ASCII code 7
    "b" -> "\b",     // backspace, ASCII code 8
    "t" -> "\t",     // horizontal tab, ASCII code 9
    "n" -> "\n",     // newline, ASCII code 10
    "v" -> "\u000b", // vertical tab, ASCII code 11 = 0o13
    "f" -> "\u000c", // form feed, ASCII code 12 = 0o14
    "r" -> "\r",     // carriage return, ASCII code 13
    "e" -> "\u001b", // escape, ASCII code 27 = 0o33
    "'" -> "'",      // single quote
    "\"" -> "\"",    // double quote
    "\\" -> "\\"     // backslash
  )
  val VALID_QUOTED = QUOTED_CC.keys.toList.sorted.mkString
  val quotedchar = P( CharIn(VALID_QUOTED).! ).map(QUOTED_CC)
  val quotedoctal: P[String] = P( octdigit.rep(1).! ).map { (digits) =>
    val code = Integer.parseInt(digits, 8).toChar
    Character.toString(code)
  }
  val quotedhex: P[String] = P( "u" ~/ hexdigit.rep(exactly = 4).! ).map { (digits) =>
    val code = Integer.parseInt(digits, 16).toChar
    Character.toString(code)
  }
  // FIXME: fix this mess with octdigit / hexdigit allowing underscore
  // probably underscore shouldn't be inside them, but somehow added separately
  // plus there's a problem with "0x_" and "0o_" being legal now

  val integer: P[BigInt] = P( octinteger | hexinteger | bininteger | decimalinteger)
  val decimalinteger: P[BigInt] = P( nonzerodigit ~ (digit | "_").rep | "0" ).!.map(parseNum(_, 10))
  val octinteger: P[BigInt] = P( "0" ~ ("o" | "O") ~ octdigit.rep(1).! ).map(parseNum(_, 8))
  val hexinteger: P[BigInt] = P( "0" ~ ("x" | "X") ~ hexdigit.rep(1).! ).map(parseNum(_, 16))
  val bininteger: P[BigInt] = P( "0" ~ ("b" | "B") ~ bindigit.rep(1).! ).map(parseNum(_, 2))
  val nonzerodigit: P0 = P( CharIn('1' to '9') )
  val octdigit: P0 = P( CharIn('0' to '7') | "_" )
  val bindigit: P0 = P( "0" | "1" | "_" )
  val hexdigit: P0 = P( digit | CharIn('a' to 'f', 'A' to 'F') | "_" )

  val floatnumber: P[BigDecimal] = P(
      digit.rep(1) ~ exponent // Ex.: 4E2, 4E+2, 4e-2
    | fixed ~ exponent.?      // Ex.: 4.E2, .4e+2, 4.2e-0
  ).!.map(BigDecimal(_))
  val fixed = P(
      digit.rep ~ "." ~ digit.rep(1)                // Ex.: 4.2, .42
    | digit.rep(1) ~ "." ~ !(wscomment ~ nameStart) // Ex.: 42., but not '42.abc' or '42.  def'
  )
  val exponent: P0 = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )

  /**
    * Converts number literal from string form into BigInt, ignoring underscores that might be
    * inside (usually for the sake of digit ranges separation).
    * @param literal number literal as a string (i.e. "1234", "12_34" or "caf_e")
    * @param base conversion base (10 = decimal, 8 = octal, 16 = hex, 2 = binary, etc)
    * @return number literal as BigInt
    */
  def parseNum(literal: String, base: Int): BigInt = {
    val cleanLiteral = literal.replace("_", "")
    scala.BigInt(cleanLiteral, base)
  }
}
