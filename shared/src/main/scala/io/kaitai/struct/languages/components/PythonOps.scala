package io.kaitai.struct.languages.components

import io.kaitai.struct.StringLanguageOutputWriter
import io.kaitai.struct.format.{DocSpec, TextRef, UrlRef}

object PythonOps {
  def compileUniversalDocs(doc: DocSpec): String = {
    val docStr = doc.summary match {
      case Some(summary) =>
        val lastChar = summary.last
        if (lastChar == '.' || lastChar == '\n') {
          summary
        } else {
          summary + "."
        }
      case None =>
        ""
    }

    val extraNewline = if (docStr.isEmpty || docStr.last == '\n') "" else "\n"
    val refStr = doc.ref.map {
      case TextRef(text) =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", text)
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
      case ref: UrlRef =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", s"${ref.text} - ${ref.url}")
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
    }.mkString("\n")

    docStr + refStr
  }
}
