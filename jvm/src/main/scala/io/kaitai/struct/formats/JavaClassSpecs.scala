package io.kaitai.struct.formats

import io.kaitai.struct.Log
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/**
  * Java implementation of ClassSpec container, doing imports from local files.
  */
class JavaClassSpecs(relPath: String) extends ClassSpecs {
  private val relFiles = mutable.Map[List[String], ClassSpec]()
  private val absFiles = mutable.Map[List[String], ClassSpec]()

  override def importRelative(name: List[String]): Future[Option[ClassSpec]] = {
    Future {
      Log.importOps.info(() => s".. importing relative $name")
      // Have we loaded it previously?
      relFiles.get(name) match {
        case Some(_) =>
          // Yes, it's already loaded and processed, nothing new here
          Log.importOps.info(() => s".... cached")
          None
        case None =>
          // Nope, let's import it
          val spec = JavaKSYParser.fileNameToSpec(relPath + "/" + name.mkString("/") + ".ksy")
          relFiles(name) = spec
          Some(spec)
      }
    }(ExecutionContext.global)
  }

  override def importAbsolute(name: List[String]): Future[Option[ClassSpec]] = ???
}
