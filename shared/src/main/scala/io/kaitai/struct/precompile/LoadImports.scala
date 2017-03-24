package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Precompilation stage that manages loading of extra .ksy files requested in
  * `meta/imports` key of initial .ksy file.
  *
  * @param specs collection of [[ClassSpec]] entries to work on
  */
class LoadImports(specs: ClassSpecs) {
  /**
    * Recursively loads and processes all .ksy files referenced in
    * `meta/import` section of given class spec and all nested classes.
    * Starts with a single given class, and recursively processes
    * both classes that will be loaded due to import, and nested classes
    * into given class.
    *
    * @param curClass class spec to start recursive import from
    */
  def processClass(curClass: ClassSpec): Future[List[ClassSpec]] = {
    Log.importOps.info(() => s".. LoadImports: processing class ${curClass.nameAsStr}")

    val thisMetaFuture: Future[List[ClassSpec]] = curClass.meta match {
      case Some(meta) =>
        Future.sequence(meta.imports.zipWithIndex.map { case (name, idx) =>
          loadImport(name, meta.path ++ List("imports", idx.toString), Some(curClass.nameAsStr))
        }).map((x) => x.flatten)
      case None =>
        Future { List() }
    }

    val nestedFuture: Future[Iterable[ClassSpec]] = Future.sequence(curClass.types.map({
      case (_, nestedClass) => processClass(nestedClass)
    })).map((listOfLists) => listOfLists.flatten)

    Future.sequence(List(thisMetaFuture, nestedFuture)).map((x) => x.flatten)
  }

  private def loadImport(name: String, path: List[String], inFile: Option[String]): Future[List[ClassSpec]] = {
    val futureSpec = if (name.startsWith("/")) {
      specs.importAbsolute(name.substring(1), path, inFile)
    } else {
      specs.importRelative(name, path, inFile)
    }

    futureSpec.flatMap { case optSpec =>
      optSpec match {
        case Some(spec) =>
          val specName = spec.name.head
          // Check if we've already had this spec in our ClassSpecs. If we do,
          // don't do anything: we've already processed it and reprocessing it
          // might lead to infinite recursion.
          //
          // In theory, duplicate imports shouldn't be returned at all by
          // import* methods due to caching, but we won't rely on it here.
          if (!specs.contains(specName)) {
            specs(specName) = spec
            processClass(spec)
          } else {
            Future { List() }
          }
        case None =>
          Future { List() }
      }
    }
  }
}
