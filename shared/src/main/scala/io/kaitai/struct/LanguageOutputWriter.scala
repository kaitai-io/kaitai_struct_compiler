package io.kaitai.struct

abstract class LanguageOutputWriter(indentStr: String) {
  var indentLevel = 0

  def inc: Unit = indentLevel += 1
  def dec: Unit = indentLevel -= 1
  def indentNow: String = indentStr * indentLevel

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
  private var sb = new StringBuilder

  def result = sb.toString

  /** Capture prerequisite statements without emitting them yet. The same writer
    * remains in use by callers, including nested captures and compiler overrides.
    * Captured lines retain their indentation; the caller chooses when to add them.
    */
  def capture[T](body: => T): (T, StringLanguageOutputWriter) = {
    val previousBuffer = sb
    val previousIndent = indentLevel
    val captured = new StringLanguageOutputWriter(indentStr)
    sb = captured.sb
    try {
      (body, captured)
    } finally {
      sb = previousBuffer
      indentLevel = previousIndent
    }
  }

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
