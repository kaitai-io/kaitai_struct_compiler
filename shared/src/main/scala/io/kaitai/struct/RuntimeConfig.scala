package io.kaitai.struct

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
  goPackage: String = "",
  javaPackage: String = "",
  javaFromFileClass: String = "io.kaitai.struct.ByteBufferKaitaiStream",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = "",
  pythonPackage: String = ""
)
