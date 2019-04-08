package io.kaitai.struct.format

case class EnumSpec(map: Map[Long, EnumValueSpec]) {
  var name = List[String]()

  /**
    * @return Absolute name of enum as string, components separated by
    *         double colon operator `::`
    */
  def nameAsStr = name.mkString("::")

  /**
    * Stabilize order of generated enums by sorting it by integer ID - it
    * both looks nicer and doesn't screw diffs in generated code.
    */
  lazy val sortedSeq: Seq[(Long, EnumValueSpec)] = map.toSeq.sortBy(_._1)
}

object EnumSpec {
  def fromYaml(src: Any, path: List[String]): EnumSpec = {
    val srcMap = ParseUtils.asMap(src, path)
    EnumSpec(srcMap.map { case (id, desc) =>
      val idLong = ParseUtils.asLong(id, path)
      val value = EnumValueSpec.fromYaml(desc, path ++ List(idLong.toString))

      idLong -> value
    })
  }
}
