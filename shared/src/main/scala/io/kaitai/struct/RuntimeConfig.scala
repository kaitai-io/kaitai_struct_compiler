package io.kaitai.struct

/**
  * C++-specific runtime configuration of the compiler.
  * @param namespace C++ namespace to generate classes in.
  * @param usePragmaOnce If true, use `#pragma once` in headers. If false (default),
  *                      use `#ifndef`-`#define`-`#endif` guards.
  * @param stdStringFrontBack If true, allow use of `front()` and `back()` methods
  *                           on `std::string`. If false, come up with simulation.
  * @param useListInitializers If true, allows use of list initializers for
  *                            `std::vector` and `std::set`. Otherwise, throw a fatal
  *                            "not implemented" error.
  * @param pointers Choose which style of pointers to use.
  */
case class CppRuntimeConfig(
  namespace: List[String] = List(),
  usePragmaOnce: Boolean = false,
  stdStringFrontBack: Boolean = false,
  useListInitializers: Boolean = false,
  pointers: CppRuntimeConfig.Pointers = CppRuntimeConfig.RawPointers
) {
  /**
    * Copies this C++ runtime config, applying all the default settings for
    * C++98 target.
    */
  def copyAsCpp98() = copy(
    usePragmaOnce = false,
    stdStringFrontBack = false,
    useListInitializers = false,
    pointers = CppRuntimeConfig.RawPointers
  )

  /**
    * Copies this C++ runtime config, applying all the default settings for
    * C++11 target.
    */
  def copyAsCpp11() = copy(
    usePragmaOnce = true,
    stdStringFrontBack = true,
    useListInitializers = true,
    pointers = CppRuntimeConfig.UniqueAndRawPointers
  )
}

object CppRuntimeConfig {
  sealed trait Pointers
  case object RawPointers extends Pointers
  case object UniqueAndRawPointers extends Pointers
}

/**
  * Java-specific runtime configuration of the compiler.
  * @param javaPackage Package to generate classes in.
  * @param fromFileClass Class to be invoked in `fromFile` helper methods.
  * @param endOfStreamErrorClass Exception class expected to be thrown on
  *                              end-of-stream errors.
  */
case class JavaRuntimeConfig(
  javaPackage: String = "",
  fromFileClass: String = "io.kaitai.struct.ByteBufferKaitaiStream",
  endOfStreamErrorClass: String = "java.nio.BufferUnderflowException",
)

/**
  * Runtime configuration of the compiler which controls certain aspects of
  * code generation for target languages.
  * @param autoRead If true, constructor (or equivalent) invocation would
  *                 automatically run `_read` (or equivalent), thus allowing to
  *                 run parsing just by constructing an object, passing a stream
  *                 into it. If false, `_read` would be made public and it is
  *                 expected to be invoked manually.
  * @param readStoresPos If true, parser (`_read` or equivalent) will store
  *                      positions of all the attributes relative to the stream;
  *                      not required for production usage (as it is typically slow
  *                      and memory-consuming), but it is crucial for visualizers,
  *                      IDEs, etc, to be able to display data layout.
  * @param opaqueTypes If true, invoking any unknown type will be treated as it was
  *                    "opaque" type, i.e. an external KaitaiStruct-compatible type
  *                    defined somewhere else. If false, it will be reported as
  *                    precompile error.
  * @param zeroCopySubstream If true, code generated will handle substreams using
  *                          zero-copy semantics if possible (with is faster and
  *                          uses less memory, but the downside is that the stream
  *                          must be seekable). If false, it will always use reading
  *                          in raw byte arrays and then constructing new substream
  *                          off the in-memory content.
  * @param cppConfig C++-specific configuration
  * @param java Java-specific configuration
  * @param goPackage Go package name
  * @param dotNetNamespace .NET (C#) namespace
  * @param phpNamespace PHP namespace
  * @param pythonPackage Python package name
  * @param nimModule Path of Nim runtime module
  * @param nimOpaque Directory of opaque Nim modules
  */
case class RuntimeConfig(
  autoRead: Boolean = true,
  readStoresPos: Boolean = false,
  opaqueTypes: Boolean = false,
  zeroCopySubstream: Boolean = true,
  cppConfig: CppRuntimeConfig = CppRuntimeConfig(),
  goPackage: String = "",
  java: JavaRuntimeConfig = JavaRuntimeConfig(),
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = "",
  pythonPackage: String = "",
  nimModule: String = "kaitai_struct_nim_runtime",
  nimOpaque: String = ""
)
