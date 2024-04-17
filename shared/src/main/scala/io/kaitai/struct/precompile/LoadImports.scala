package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Precompilation stage that manages loading of extra .ksy files requested in
  * `meta/imports` key of initial .ksy file.
  *
  * @param specs collection of [[format.ClassSpec]] entries to work on
  */
class LoadImports(specs: ClassSpecs) {
  import LoadImports._

  /**
    * Recursively loads and processes all .ksy files referenced in
    * `meta/import` section of given class spec and all nested classes.
    * Starts with a single given class, and recursively processes
    * both classes that will be loaded due to import, and nested classes
    * into given class.
    *
    * @param curClass class spec to start recursive import from
    */
  def processClass(curClass: ClassSpec, workDir: ImportPath): Future[List[ClassSpec]] = {
    Log.importOps.info(() => s".. LoadImports: processing class ${curClass.nameAsStr} (workDir = $workDir)")

    val thisMetaFuture: Future[List[ClassSpec]] =
      Future.sequence(curClass.meta.imports.zipWithIndex.map { case (name, idx) =>
        loadImport(
          name,
          curClass.meta.path ++ List("imports", idx.toString),
          Some(curClass.fileNameAsStr),
          workDir
        )
      }).map((x) => x.flatten)

    val nestedFuture: Future[Iterable[ClassSpec]] = Future.sequence(curClass.types.map({
      case (_, nestedClass) => processClass(nestedClass, workDir)
    })).map((listOfLists) => listOfLists.flatten)

    Future.sequence(List(thisMetaFuture, nestedFuture)).map((x) => x.flatten)
  }

  private def loadImport(name: String, path: List[String], inFile: Option[String], workDir: ImportPath): Future[List[ClassSpec]] = {
    Log.importOps.info(() => s".. LoadImports: loadImport($name, workDir = $workDir)")

    val impPath = ImportPath.fromString(name)
    val fullPath = ImportPath.add(workDir, impPath)

    val futureSpec = fullPath match {
      case RelativeImportPath(p) =>
        specs.importRelative(p.mkString("/"), path, inFile)
      case AbsoluteImportPath(p) =>
        specs.importAbsolute(p.mkString("/"), path, inFile)
    }

    futureSpec.flatMap { case optSpec =>
      Log.importOps.info(() => {
        val specNameAsStr = optSpec.map(_.nameAsStr).getOrElse("<none>")
        s".. LoadImports: loadImport($name, workDir = $workDir), got spec=$specNameAsStr"
      })
      optSpec match {
        case Some(spec) =>
          val specName = spec.name.head
          // Check if spec name does not match file name. If it doesn't match,
          // it is probably already a serious error.
          if (name != specName)
            Log.importOps.warn(() => s"... expected to have type name $name, but got $specName")

          // Check if we've already had this spec in our ClassSpecs. If we do,
          // don't do anything: we've already processed it and reprocessing it
          // might lead to infinite recursion.
          //
          // In theory, duplicate imports shouldn't be returned at all by
          // import* methods due to caching, but we won't rely on it here.
          //
          // The `synchronized` block is necessary because this code is run
          // concurrently by multiple threads (each resolving different imports)
          // and `specs` is a shared non-thread-safe `HashMap`. Without this
          // synchronization, a few imports were occasionally missing from
          // `specs` due to a race condition, and even (though rarely) the
          // implementation of `specs.contains()` could fail internally with an
          // `ArrayIndexOutOfBoundsException`. For more details, see
          // https://github.com/kaitai-io/kaitai_struct/issues/951
          val isNewSpec = specs.synchronized {
            val isNew = !specs.contains(specName)
            if (isNew) {
              specs(specName) = spec
            }
            isNew
          }
          if (isNewSpec) {
            processClass(spec, ImportPath.updateWorkDir(workDir, impPath))
          } else {
            Log.importOps.warn(() => s"... we have that already, ignoring")
            Future { List() }
          }
        case None =>
          Future { List() }
      }
    }
  }
}

object LoadImports {
  sealed trait ImportPath {
    def baseDir: ImportPath
  }
  case class RelativeImportPath(path: List[String]) extends ImportPath {
    override def baseDir: ImportPath = RelativeImportPath(path.init)
  }
  case class AbsoluteImportPath(path: List[String]) extends ImportPath {
    override def baseDir: ImportPath = AbsoluteImportPath(path.init)
  }
  val BasePath = RelativeImportPath(List())

  object ImportPath {
    def fromString(s: String): ImportPath = if (s.startsWith("/")) {
      AbsoluteImportPath(s.substring(1).split("/", -1).toList)
    } else {
      RelativeImportPath(s.split("/", -1).toList)
    }

    def add(curWorkDir: ImportPath, newPath: ImportPath): ImportPath = {
      (curWorkDir, newPath) match {
        case (_, AbsoluteImportPath(newPathAbs)) =>
          AbsoluteImportPath(newPathAbs)
        case (RelativeImportPath(curDir), RelativeImportPath(newPathRel)) =>
          RelativeImportPath(curDir ++ newPathRel)
        case (AbsoluteImportPath(curDir), RelativeImportPath(newPathRel)) =>
          AbsoluteImportPath(curDir ++ newPathRel)
      }
    }

    def updateWorkDir(curWorkDir: ImportPath, newPath: ImportPath): ImportPath =
      add(curWorkDir, newPath).baseDir
  }
}
