package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, ClassSpecs, GenericStructClassSpec, MetaSpec}
import io.kaitai.struct.languages.{GoCompiler, NimCompiler, RustCompiler}
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.precompile._
import io.kaitai.struct.problems.CompilationProblem

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  /**
    * Takes a freshly made [[format.ClassSpecs]] container with a single .ksy loaded
    * into it, launches recursive loading of imports into this container,
    * and then runs precompilation on every class that happens to be there
    * after imports.
    * @param specs [[format.ClassSpecs]] container with first type loaded into it
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
    * Runs precompilation steps on every type in the given [[format.ClassSpecs]] collection,
    * using provided configuration.
    *
    * @param specs [[format.ClassSpecs]] container with all types loaded into it
    * @param config runtime configuration to be passed to precompile steps
    * @return a list of compilation problems encountered during precompilation steps
    */
  def precompile(specs: ClassSpecs, config: RuntimeConfig): Iterable[CompilationProblem] = {
    specs.flatMap { case (_, classSpec) =>
      precompile(specs, classSpec, config)
    }
  }

  /**
    * Does all precompiles steps on a single [[format.ClassSpec]] using provided configuration.
    * See individual precompile steps invocations for more in-depth description of
    * what each step includes.
    *
    * @param classSpecs [[format.ClassSpecs]] container with all types loaded into it
    * @param topClass one top type to precompile
    * @param conf runtime configuration to be passed to precompile steps
    * @return a list of compilation problems encountered during precompilation steps
    */
  def precompile(classSpecs: ClassSpecs, topClass: ClassSpec, conf: RuntimeConfig): Iterable[CompilationProblem] = {
    val config = updateConfigFromMeta(conf, topClass.meta)

    new MarkupClassNames(classSpecs).run()
    val resolveTypeProblems = new ResolveTypes(classSpecs, config.opaqueTypes).run()

    // For now, bail out early in case we have any type resolution problems
    // TODO: try to recover and do some more passes even in face of these
    if (resolveTypeProblems.nonEmpty) {
      return resolveTypeProblems
    }

    new ParentTypes(classSpecs).run()
    new SpecsValueTypeDerive(classSpecs).run()
    new CalculateSeqSizes(classSpecs).run()
    val typeValidatorProblems = new TypeValidator(classSpecs, topClass).run()

    // Warnings
    val styleWarnings = new StyleCheckIds(classSpecs, topClass).run()
    val encodingProblems = new CanonicalizeEncodingNames(classSpecs).run()

    topClass.parentClass = GenericStructClassSpec

    resolveTypeProblems ++ typeValidatorProblems ++ styleWarnings ++ encodingProblems
  }

  /**
    * Compiles a single [[format.ClassSpec]] into a single target language using
    * provided configuration.
    * @param specs bundle of class specifications (used to search to references there)
    * @param spec class specification to compile
    * @param lang specifies which language compiler will be used
    * @param conf runtime compiler configuration
    * @return a container that contains all compiled files and results
    */
  def compile(specs: ClassSpecs, spec: ClassSpec, lang: LanguageCompilerStatic, conf: RuntimeConfig): CompileLog.SpecSuccess = {
    val config = updateConfigFromMeta(conf, spec.meta)

    val cc = lang match {
      case GraphvizClassCompiler =>
        new GraphvizClassCompiler(specs, spec)
      case GoCompiler =>
        new GoClassCompiler(specs, spec, config)
      case RustCompiler =>
        new RustClassCompiler(specs, spec, config)
      case ConstructClassCompiler =>
        new ConstructClassCompiler(specs, spec)
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
    * Currently used to enforce:
    * * debug when "ks-debug: true" is specified in top-level "meta" key
    * * zero copy stream usage when "ks-zero-copy-stream" is specified
    *
    * @param config original runtime configuration
    * @param meta meta spec for top-level type
    * @return updated runtime configuration with applied enforcements
    */
  private def updateConfigFromMeta(config: RuntimeConfig, meta: MetaSpec): RuntimeConfig = {
    val config1 = if (meta.forceDebug) {
      config.copy(autoRead = false, readStoresPos = true)
    } else {
      config
    }

    val config2 = meta.zeroCopySubstream match {
      case Some(value) => config1.copy(zeroCopySubstream = value)
      case None => config1
    }

    val config3 = meta.opaqueTypes match {
      case Some(value) => config2.copy(opaqueTypes = value)
      case None => config2
    }

    config3
  }
}
