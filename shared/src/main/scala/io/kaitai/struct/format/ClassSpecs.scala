package io.kaitai.struct.format

import scala.collection.mutable
import scala.concurrent.Future

/**
  * Top-level abstract container for all ClassSpecs. Used for recursive
  * loading of imports. Real-life implementation depend on file handling
  * (which at least differ between JVM vs JS), and thus implementations
  * are platform-dependent.
  */
abstract class ClassSpecs(val firstSpec: ClassSpec) extends mutable.HashMap[String, ClassSpec] {
  this(firstSpec.name.head) = firstSpec

  /**
    * Calls certain function on all [[ClassSpec]] elements stored in this ClassSpecs,
    * and all subtypes stored in these elements, recursively.
    */
  def forEachRec(proc: (ClassSpec) => ()): Unit = {
    // FIXME: grab exception and rethrow more localized one, with a specName?
    foreach { case (_, typeSpec) => typeSpec.forEachRec(proc) }
  }

  def importRelative(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]]
  def importAbsolute(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]]
}
