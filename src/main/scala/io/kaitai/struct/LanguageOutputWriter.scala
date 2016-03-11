package io.kaitai.struct

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

abstract class LanguageOutputWriter(indentStr: String) {
  var indentLevel = 0

  def inc: Unit = indentLevel += 1
  def dec: Unit = indentLevel -= 1
  def indentNow: String = indentStr * indentLevel

  def puts(s: String): Unit
  def puts: Unit
  def close: Unit
}

class FileLanguageOutputWriter(fileName: String, indentStr: String) extends LanguageOutputWriter(indentStr) {
  private val outDir = new File(fileName).getParentFile()
  outDir.mkdirs
  private val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)

  override def puts(s: String): Unit = out.println(indentNow + s)
  override def puts: Unit = out.println
  override def close = out.close
}

class StringLanguageOutputWriter(indentStr: String) extends LanguageOutputWriter(indentStr) {
  private val sb = new StringBuilder

  def result = sb.toString

  override def puts(s: String): Unit = {
    sb.append(indentNow)
    sb.append(s)
    sb.append("\n")
  }
  override def puts: Unit = sb.append("\n")
  override def close = {}
}
