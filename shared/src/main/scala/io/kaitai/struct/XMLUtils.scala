package io.kaitai.struct

/**
  * Copyright (c) 2002-2017 EPFL
  * Copyright (c) 2011-2017 Lightbend, Inc.
  *
  * All rights reserved.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions are met:
  *
  * * Redistributions of source code must retain the above copyright notice,
  *   this list of conditions and the following disclaimer.
  * * Redistributions in binary form must reproduce the above copyright notice,
  *   this list of conditions and the following disclaimer in the documentation
  *   and/or other materials provided with the distribution.
  * * Neither the name of the EPFL nor the names of its contributors may be
  *   used to endorse or promote products derived from this software without
  *   specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  * THE POSSIBILITY OF SUCH DAMAGE.
  */
object XMLUtils {
  // https://github.com/scala/scala-xml/blob/d26ff2826df4ed616b2bf30eae4dd02e964323fd/shared/src/main/scala/scala/xml/Utility.scala#L86-L144

  /**
    * Escapes the characters &lt; &gt; &amp; and &quot; from string.
    */
  final def escape(text: String): String = sbToString(escape(text, _))

  object Escapes {
    /**
      * For reasons unclear escape and unescape are a long ways from
      * being logical inverses.
      */
    val pairs = Map(
      "lt" -> '<',
      "gt" -> '>',
      "amp" -> '&',
      "quot" -> '"'
      // enigmatic comment explaining why this isn't escaped --
      // is valid xhtml but not html, and IE doesn't know it, says jweb
      // "apos"  -> '\''
    )
    val escMap = pairs map { case (s, c) => c -> ("&%s;" format s) }
    val unescMap = pairs ++ Map("apos" -> '\'')
  }
  import Escapes.{ escMap, unescMap }

  /**
    * Appends escaped string to `s`.
    */
  final def escape(text: String, s: StringBuilder): StringBuilder = {
    // Implemented per XML spec:
    // https://www.w3.org/International/questions/qa-controls
    // imperative code 3x-4x faster than current implementation
    // dpp (David Pollak) 2010/02/03
    val len = text.length
    var pos = 0
    while (pos < len) {
      text.charAt(pos) match {
        case '<'  => s.append("&lt;")
        case '>'  => s.append("&gt;")
        case '&'  => s.append("&amp;")
        case '"'  => s.append("&quot;")
        case '\n' => s.append('\n')
        case '\r' => s.append('\r')
        case '\t' => s.append('\t')
        case c    => if (c >= ' ') s.append(c)
      }

      pos += 1
    }
    s
  }

  /**
    * Appends unescaped string to `s`, `amp` becomes `&amp;`,
    * `lt` becomes `&lt;` etc..
    *
    * @return    `'''null'''` if `ref` was not a predefined entity.
    */
  final def unescape(ref: String, s: StringBuilder): StringBuilder =
    ((unescMap get ref) map (s append _)).orNull

  // helper for the extremely oft-repeated sequence of creating a
  // StringBuilder, passing it around, and then grabbing its String.
  private def sbToString(f: (StringBuilder) => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}
