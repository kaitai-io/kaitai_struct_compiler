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
