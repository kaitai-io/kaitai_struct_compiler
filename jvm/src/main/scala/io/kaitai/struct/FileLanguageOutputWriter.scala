package io.kaitai.struct

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

class FileLanguageOutputWriter(fileName: String, indentStr: String) extends LanguageOutputWriter(indentStr) {
  private val outDir = new File(fileName).getParentFile()
  outDir.mkdirs
  private val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)

  override def add(other: StringLanguageOutputWriter) = out.write(other.result)
  override def puts(s: String): Unit = out.println(indentNow + s)
  override def puts: Unit = out.println
  override def close = out.close
}
