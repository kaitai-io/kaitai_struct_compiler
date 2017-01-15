package io.kaitai.struct.languages.components

/**
  * Created by KT on 2017.01.15..
  */
trait PropertyNameConflict {
  def resolveInnerClassConflict(oldName: List[String]): List[String]
  def resolveInnerEnumConflict(oldName: List[String]): List[String]
}
