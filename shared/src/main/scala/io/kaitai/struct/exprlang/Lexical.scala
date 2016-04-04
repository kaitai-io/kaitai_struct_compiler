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
  def kw(s: String) = s ~ !(letter | digit | "_")

  val wscomment = P( (CharsWhile(" \n".toSet, min = 1) | "\\\n").rep )

  val identifier: P[Ast.identifier] =
    P( (letter|"_") ~ (letter | digit | "_").rep ).!.map(Ast.identifier)
  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  val stringliteral: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  val escapeseq: P0 = P( "\\" ~ AnyChar )


  val integer: P[BigInt] = P( octinteger | hexinteger | bininteger | decimalinteger)
  val decimalinteger: P[BigInt] = P( nonzerodigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))
  val octinteger: P[BigInt] = P( "0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).! ).map(scala.BigInt(_, 8))
  val hexinteger: P[BigInt] = P( "0" ~ ("x" | "X") ~ hexdigit.rep(1).! ).map(scala.BigInt(_, 16))
  val bininteger: P[BigInt] = P( "0" ~ ("b" | "B") ~ bindigit.rep(1).! ).map(scala.BigInt(_, 2))
  val nonzerodigit: P0 = P( CharIn('1' to '9') )
  val octdigit: P0 = P( CharIn('0' to '7') )
  val bindigit: P0 = P( "0" | "1" )
  val hexdigit: P0 = P( digit | CharIn('a' to 'f', 'A' to 'F') )


  val floatnumber: P[BigDecimal] = P( pointfloat | exponentfloat )
  val pointfloat: P[BigDecimal] = P( intpart.? ~ fraction | intpart ~ "." ).!.map(BigDecimal(_))
  val exponentfloat: P[BigDecimal] = P( (intpart | pointfloat) ~ exponent ).!.map(BigDecimal(_))
  val intpart: P[BigDecimal] = P( digit.rep(1) ).!.map(BigDecimal(_))
  val fraction: P0 = P( "." ~ digit.rep(1) )
  val exponent: P0 = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )
}
