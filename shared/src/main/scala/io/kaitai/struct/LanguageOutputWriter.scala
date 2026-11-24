package io.kaitai.struct

abstract class LanguageOutputWriter(indentStr: String) {
  var indentLevel = 0

  /** Indent strings by level, built on demand, so that `puts` doesn't rebuild them on every line. */
  private val indents = scala.collection.mutable.ArrayBuffer("")

  def inc: Unit = indentLevel += 1
  def dec: Unit = indentLevel -= 1
  def indentNow: String = {
    if (indentLevel <= 0)
      return ""
    while (indents.length <= indentLevel)
      indents += indents.last + indentStr
    indents(indentLevel)
  }

  def add(other: StringLanguageOutputWriter): Unit
  def puts(s: String): Unit
  def puts: Unit
  def close: Unit

  /**
    * Utility method that outputs several lines at once, splitting
    * input by newline. Useful to print out multi-line comments,
    * as for docstrings.
    * @param prefix prefix to prepend to every line
    * @param lines lines as a string, joined by newline
    * @param hanging prefix to prepend to every line except for the first
    *                (i.e. hanging indent)
    */
  def putsLines(prefix: String, lines: String, hanging: String = ""): Unit = {
    val strs = lines.split("\n")
    puts(s"$prefix${strs(0)}")
    strs.drop(1).foreach((line) => puts(s"$prefix$hanging$line"))
  }
}

class StringLanguageOutputWriter(indentStr: String) extends LanguageOutputWriter(indentStr) {
  private val sb = new StringBuilder

  def result = sb.toString

  override def add(other: StringLanguageOutputWriter) = sb.append(other.result)
  override def puts(s: String): Unit = {
    sb.append(indentNow)
    sb.append(s)
    sb.append("\n")
  }
  override def puts: Unit = sb.append("\n")
  override def close = {}

  def clear() = sb.clear()
}
