package io.kaitai.struct

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

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
    */
  def putsLines(prefix: String, lines: String): Unit = {
    lines.split("\n").foreach((line) =>
      puts(s"$prefix$line")
    )
  }
}

class FileLanguageOutputWriter(fileName: String, indentStr: String) extends LanguageOutputWriter(indentStr) {
  private val outDir = new File(fileName).getParentFile()
  outDir.mkdirs
  private val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)

  override def add(other: StringLanguageOutputWriter) = out.write(other.result)
  override def puts(s: String): Unit = out.println(indentNow + s)
  override def puts: Unit = out.println
  override def close = out.close
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
