package io.kaitai.struct.format

import io.kaitai.struct.XMLUtils

sealed trait RefSpec
case class TextRef(text: String) extends RefSpec
case class UrlRef(url: String, text: String) extends RefSpec {
  /**
    * Converts contents of this URL reference to a commonly encountered
    * a-href style link.
    * @return string with HTML rendition of A tag with href attribute
    */
  def toAhref: String =
    "<a href=\"" + XMLUtils.escape(url) + "\">" + text + "</a>"
}

case class DocSpec(
  summary: Option[String],
  ref: List[RefSpec]
) {
  def isEmpty: Boolean = summary.isEmpty && ref.isEmpty
}

object DocSpec {
  val EMPTY = DocSpec(None, List())

  def fromYaml(srcMap: Map[String, Any], path: List[String]): DocSpec = {
    val doc = ParseUtils.getOptValueStr(srcMap, "doc", path)

    val docRefs = ParseUtils.getListStr(srcMap, "doc-ref", path)
    val refSpec = docRefs.map(docRef => parseSingleRefSpec(docRef))

    DocSpec(doc, refSpec)
  }

  def parseSingleRefSpec(docRef: String): RefSpec = {
    if (docRef.startsWith("http://") || docRef.startsWith("https://")) {
      val splitPoint = docRef.indexOf(' ')
      if (splitPoint < 0) {
        UrlRef(docRef, "Source")
      } else {
        val url = docRef.substring(0, splitPoint).trim
        val text = docRef.substring(splitPoint + 1).trim
        UrlRef(url, text)
      }
    } else {
      TextRef(docRef)
    }
  }
}
