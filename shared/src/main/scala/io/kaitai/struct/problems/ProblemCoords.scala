package io.kaitai.struct.problems

/**
  * Pointer to a problem related to specific file and/or YAML path and/or line and/or column in it.
  * @param file file to report as erroneous, None means "main compilation unit"
  * @param path YAML path in a file
  * @param line line number which triggered the error (1-based)
  * @param col column number which triggered the error (1-based)
  */
case class ProblemCoords(
  val file: Option[String] = None,
  val path: Option[List[String]] = None,
  val line: Option[Int] = None,
  val col: Option[Int] = None
) {
  def message: String = {
    ProblemCoords.formatFileName(file) +
      (line match {
        case Some(lineNum) => s":${lineNum}" + (col match {
          case Some(colNum) => s":${colNum}"
          case None => ""
        })
        case None => ""
      }) +
      path.map(pathVal => s": ${pathVal.mkString("/", "/", "")}").getOrElse("")
  }

  def toSeq: Seq[(String, Any)] = {
    Seq("file" -> ProblemCoords.formatFileName(file)) ++
      line.map(lineVal => "line" -> lineVal) ++
      col.map(colVal => "col" -> colVal) ++
      path.map(pathVal => "path" -> pathVal)
  }
}

object ProblemCoords {
  def formatFileName(file: Option[String]): String = file.getOrElse("(main)")
}
