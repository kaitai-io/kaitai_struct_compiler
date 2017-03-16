package io.kaitai.struct

import io.kaitai.struct.format.ClassSpec
import io.kaitai.struct.languages.components.LanguageCompilerStatic

object Main {
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
  def updateConfig(config: RuntimeConfig, topClass: ClassSpec): RuntimeConfig = {
    if (topClass.meta.get.forceDebug) {
      config.copy(debug = true)
    } else {
      config
    }
  }
}
