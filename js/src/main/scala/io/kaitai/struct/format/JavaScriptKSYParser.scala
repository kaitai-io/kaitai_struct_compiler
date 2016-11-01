package io.kaitai.struct.format

import scala.scalajs.js

object JavaScriptKSYParser {
  def yamlJavascriptToScala(src: Any): Any = {
    src match {
      case array: js.Array[AnyRef] =>
        array.toList.map(yamlJavascriptToScala)
      case _: String | _: Int | _: Double | _: Boolean =>
        src
      case dict =>
        dict.asInstanceOf[js.Dictionary[AnyRef]].toMap.mapValues(yamlJavascriptToScala)
    }
  }
}
