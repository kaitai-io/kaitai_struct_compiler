package io.kaitai.struct.languages.components

/**
  * Created by KT on 2017.01.15..
  */
trait InnerClassPropertyConflict {
  def resolveInnerClassNameConflict(oldName: List[String]): List[String]
}
