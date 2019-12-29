package io.kaitai.struct.languages.components

import io.kaitai.struct.ImportList

/**
  * Import list manager with C/C++-specific semantics - allows to specify
  * whether a particular file is going to be
  */
class CppImportList {
  def addSystem(fileName: String) = importList.add(s"<$fileName>")
  def addLocal(fileName: String) = importList.add("\"" + fileName + "\"")
  def addKaitai(fileName: String) = addLocal(fileName)

  def result: String =
    importList.toList.map((x) => s"#include $x").mkString("", "\n", "\n")

  private val importList = new ImportList()
}
