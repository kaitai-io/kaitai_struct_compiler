package io.kaitai.struct.format

case class EnumSpec(map: Map[Long, String]) {
  var name = List[String]()

  /**
    * Stabilize order of generated enums by sorting it by integer ID - it
    * both looks nicer and doesn't screw diffs in generated code.
    */
  lazy val sortedSeq: Seq[(Long, String)] = map.toSeq.sortBy(_._1)
}

object EnumSpec {
  def fromYaml(src: Any, path: List[String]): EnumSpec = {
    val srcMap = ParseUtils.asMap(src, path)
    EnumSpec(srcMap.map { case (id, name) =>
      val idLong = ParseUtils.asLong(id, path)
      val symbName = ParseUtils.asStr(name, path ++ List(idLong.toString))

      Identifier.checkIdentifierSource(symbName, "enum member", path ++ List(idLong.toString))

      idLong -> symbName
    })
  }
}
