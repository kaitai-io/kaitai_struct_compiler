package io.kaitai.struct.precompile

import io.kaitai.struct.format.{ClassSpec, ClassSpecs}

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
  def processClass(curClass: ClassSpec): Unit = {
    curClass.meta.foreach((meta) =>
      meta.imports.foreach((name) => loadImport(name))
    )

    curClass.types.foreach { case (_, nestedClass) =>
      processClass(nestedClass)
    }
  }

  private def loadImport(name: String): Unit = {
    val parts = name.split('/').toList
    val head::tail = parts
    val optSpec = if (head.isEmpty) {
      specs.importAbsolute(tail)
    } else {
      specs.importRelative(parts)
    }

    optSpec.foreach { (spec) =>
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
      }
    }
  }
}
