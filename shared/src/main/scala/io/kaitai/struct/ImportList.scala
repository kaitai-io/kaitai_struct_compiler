package io.kaitai.struct

import scala.collection.mutable.ListBuffer

/**
  * Manages imports/includes/requires/etc lists used for particular compilation
  * unit, makes sure they are unique.
  */
class ImportList {
  private val list = ListBuffer[String]()
  def add(s: String) = Utils.addUniqueAttr(list, s)
  def toList: List[String] = list.toList
}
