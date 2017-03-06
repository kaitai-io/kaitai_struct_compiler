package io.kaitai.struct.formats

import io.kaitai.struct.Log
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}

import scala.collection.mutable

/**
  * Java implementation of ClassSpec container, doing imports from local files.
  */
class JavaClassSpecs(relPath: String) extends ClassSpecs {
  val relFiles = mutable.Map[List[String], ClassSpec]()
  val absFiles = mutable.Map[List[String], ClassSpec]()

  override def importRelative(name: List[String]): Option[ClassSpec] = {
    Log.importOps.info(() => s".. importing relative $name")
    // Have we loaded it previously?
    relFiles.get(name) match {
      case Some(_) =>
        // Yes, it's already loaded and processed, nothing new here
        None
      case None =>
        // Nope, let's import it
        Some(JavaKSYParser.fileNameToSpec(relPath + "/" + name.mkString("/") + ".ksy"))
    }
  }

  override def importAbsolute(name: List[String]): Option[ClassSpec] = ???
}
