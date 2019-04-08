package io.kaitai.struct

/**
  * C++-specific runtime configuration of the compiler.
  * @param usePragmaOnce If true, use `#pragma once` in headers. If false (default),
  *                      use `#ifndef`-`#define`-`#endif` guards.
  * @param pointers Choose which style of pointers to use.
  */
case class CppRuntimeConfig(
  namespace: List[String] = List(),
  usePragmaOnce: Boolean = false,
  pointers: CppRuntimeConfig.Pointers = CppRuntimeConfig.RawPointers
) {
  /**
    * Copies this C++ runtime config, applying all the default settings for
    * C++98 target.
    */
  def copyAsCpp98() = copy(
    usePragmaOnce = false,
    pointers = CppRuntimeConfig.RawPointers
  )

  /**
    * Copies this C++ runtime config, applying all the default settings for
    * C++11 target.
    */
  def copyAsCpp11() = copy(
    usePragmaOnce = true,
    pointers = CppRuntimeConfig.UniqueAndRawPointers
  )
}

object CppRuntimeConfig {
  sealed trait Pointers
  case object RawPointers extends Pointers
  case object SharedPointers extends Pointers
  case object UniqueAndRawPointers extends Pointers
}

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
  * @param cppConfig C++-specific configuration
  * @param goPackage Go package name
  * @param javaPackage Java package name
  * @param javaFromFileClass Java class to be invoked in `fromFile` helper methods
  * @param dotNetNamespace .NET (C#) namespace
  * @param phpNamespace PHP namespace
  * @param pythonPackage Python package name
  */
case class RuntimeConfig(
  autoRead: Boolean = true,
  readStoresPos: Boolean = false,
  opaqueTypes: Boolean = false,
  cppConfig: CppRuntimeConfig = CppRuntimeConfig(),
  goPackage: String = "",
  javaPackage: String = "",
  javaFromFileClass: String = "io.kaitai.struct.ByteBufferKaitaiStream",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = "",
  pythonPackage: String = ""
)
