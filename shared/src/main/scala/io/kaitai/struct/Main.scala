package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, ClassSpecs, GenericStructClassSpec}
import io.kaitai.struct.languages.{GoCompiler, NimCompiler, RustCompiler}
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.precompile._
import io.kaitai.struct.problems.CompilationProblem

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  /**
    * Takes a freshly made [[ClassSpecs]] container with a single .ksy loaded
    * into it, launches recursive loading of imports into this container,
    * and then runs precompilation on every class that happens to be there
    * after imports.
    * @param specs [[ClassSpecs]] container with first type loaded into it
    * @param config runtime configuration to be passed to precompile step
    * @return a future that will resolve when both imports and precompilations
    *         are complete; modifies given container by loading extra classes
    *         into it and modifying classes itself by precompilation step
    */
  def importAndPrecompile(specs: ClassSpecs, config: RuntimeConfig): Future[Iterable[CompilationProblem]] = {
    new LoadImports(specs).processClass(specs.firstSpec, LoadImports.BasePath).map { (allSpecs) =>
      Log.importOps.info(() => s"imports done, got: ${specs.keys} (async=$allSpecs)")

      precompile(specs, config)
    }
  }

  /**
    * Runs precompilation steps on every type in the given [[ClassSpecs]] collection,
    * using provided configuration.
    *
    * @param specs [[ClassSpecs]] container with all types loaded into it
    * @param config runtime configuration to be passed to precompile steps
    * @return a list of compilation problems encountered during precompilation steps
    */
  def precompile(specs: ClassSpecs, config: RuntimeConfig): Iterable[CompilationProblem] = {
    specs.flatMap { case (_, classSpec) =>
      precompile(specs, classSpec, config)
    }
  }

  /**
    * Does all precompiles steps on a single [[ClassSpec]] using provided configuration.
    * See individual precompile steps invocations for more in-depth description of
    * what each step includes.
    *
    * @param classSpecs [[ClassSpecs]] container with all types loaded into it
    * @param topClass one top type to precompile
    * @param config runtime configuration to be passed to precompile steps
    * @return a list of compilation problems encountered during precompilation steps
    */
  def precompile(classSpecs: ClassSpecs, topClass: ClassSpec, config: RuntimeConfig): Iterable[CompilationProblem] = {
    classSpecs.foreach { case (_, curClass) => MarkupClassNames.markupClassNames(curClass) }
    val opaqueTypes = topClass.meta.opaqueTypes.getOrElse(config.opaqueTypes)
    new ResolveTypes(classSpecs, opaqueTypes).run()
    new ParentTypes(classSpecs).run()
    new SpecsValueTypeDerive(classSpecs).run()
    new CalculateSeqSizes(classSpecs).run()
    val typeValidatorProblems = new TypeValidator(classSpecs, topClass).run()

    // Warnings
    val styleWarnings = new StyleCheckIds(classSpecs, topClass).run()

    topClass.parentClass = GenericStructClassSpec

    typeValidatorProblems ++ styleWarnings
  }

  /**
    * Compiles a single [[ClassSpec]] into a single target language using
    * provided configuration.
    * @param specs bundle of class specifications (used to search to references there)
    * @param spec class specification to compile
    * @param lang specifies which language compiler will be used
    * @param conf runtime compiler configuration
    * @return a container that contains all compiled files and results
    */
  def compile(specs: ClassSpecs, spec: ClassSpec, lang: LanguageCompilerStatic, conf: RuntimeConfig): CompileLog.SpecSuccess = {
    val config = updateConfig(conf, spec)

    val cc = lang match {
      case GraphvizClassCompiler =>
        new GraphvizClassCompiler(specs, spec)
      case GoCompiler =>
        new GoClassCompiler(specs, spec, config)
      case RustCompiler =>
        new RustClassCompiler(specs, spec, config)
      case ConstructClassCompiler =>
        new ConstructClassCompiler(specs, spec, config)
      case NimCompiler =>
        new NimClassCompiler(specs, spec, config)
      case HtmlClassCompiler =>
        new HtmlClassCompiler(specs, spec)
      case _ =>
        new ClassCompiler(specs, spec, config, lang)
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
    if (topClass.meta.forceDebug) {
      config.copy(autoRead = false, readStoresPos = true)
    } else {
      config
    }
  }
}
