package io.kaitai.struct.languages.components

import io.kaitai.struct.{ImportList, StringLanguageOutputWriter, Utils}
import io.kaitai.struct.format.ClassSpec

import scala.collection.mutable.ListBuffer

/**
  * Common trait for languages that have one output file per ClassSpec.
  * This file is considered to be composed of:
  *
  * * a header
  * * imports list
  * * output body
  */
trait SingleOutputFile extends LanguageCompiler {
  /**
    * @param topClassName name of the top-level type in standard KS notation (lower underscore).
    * @return File name supposed to host a given top class name.
    */
  def outFileName(topClassName: String): String

  val outHeader = new StringLanguageOutputWriter(indent)
  val out = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec) =
    Map(outFileName(topClass.nameAsStr) ->
      (outHeader.result + outImports(topClass) + out.result)
    )

  val importList = new ImportList

  /**
    * Generates imports clauses in target language format
    * @return import
    */
  def outImports(topClass: ClassSpec) = ""
}
