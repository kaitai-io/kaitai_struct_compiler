package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, ClassSpecs, GenericStructClassSpec}
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.precompile._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  /**
    * Takes a freshly made [[ClassSpecs]] container with a single .ksy loaded
    * into it, launches recursive loading of imports into this container,
    * and then runs precompilation on every class that happens to be there
    * after imports.
    * @param specs ClassSpecs container with first class loaded into it
    * @param config runtime configuration to be passed to precompile step
    * @return a future that will resolve when both imports and precompilations
    *         are complete; modifies given container by loading extra classes
    *         into it and modifying classes itself by precompilation step
    */
  def importAndPrecompile(specs: ClassSpecs, config: RuntimeConfig): Future[Unit] = {
    new LoadImports(specs).processClass(specs.firstSpec).map { (allSpecs) =>
      Log.importOps.info(() => s"imports done, got: ${specs.keys} (async=$allSpecs)")

      specs.foreach { case (_, classSpec) =>
        precompile(specs, classSpec, config)
      }
    }
  }

  def precompile(classSpecs: ClassSpecs, topClass: ClassSpec, config: RuntimeConfig): Unit = {
    classSpecs.foreach { case (_, curClass) => MarkupClassNames.markupClassNames(curClass) }
    val opaqueTypes = topClass.meta.get.opaqueTypes.getOrElse(config.opaqueTypes)
    new ResolveTypes(classSpecs, opaqueTypes).run()
    classSpecs.foreach { case (_, curClass) => ParentTypes.markup(curClass) }
    new SpecsValueTypeDerive(classSpecs).run()
    new TypeValidator(topClass).run()

    topClass.parentClass = GenericStructClassSpec
  }

  /**
    * Compiles a single [[ClassSpec]] into a single target language using
    * provided configuration.
    * @param spec class specification to compile
    * @param lang specifies which language compiler will be used
    * @param conf runtime compiler configuration
    * @return a container that contains all compiled files and results
    */
  def compile(spec: ClassSpec, lang: LanguageCompilerStatic, conf: RuntimeConfig): CompileLog.SpecSuccess = {
    val config = updateConfig(conf, spec)

    val cc = lang match {
      case GraphvizClassCompiler =>
        new GraphvizClassCompiler(spec)
      case _ =>
        new ClassCompiler(spec, config, lang)
    }
    cc.compile
  }

  /**
    * Updates runtime configuration with "enforcement" options that came from a source file itself.
    * Currently only used to enforce debug when "ks-debug: true" is specified in top-level "meta" key.
    * @param config original runtime configuration
    * @param topClass top-level class spec
    * @return updated runtime configuration with applied enforcements
    */
  private def updateConfig(config: RuntimeConfig, topClass: ClassSpec): RuntimeConfig = {
    if (topClass.meta.get.forceDebug) {
      config.copy(debug = true)
    } else {
      config
    }
  }
}
