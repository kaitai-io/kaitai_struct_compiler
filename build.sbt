name := "kaitai-struct-compiler"

version := "0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.github.scopt" %% "scopt" % "3.4.0",
  "com.lihaoyi" %% "fastparse" % "0.3.5",
  "org.yaml" % "snakeyaml" % "1.16",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.1.1",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.1.1",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.1",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.1.1"
)

enablePlugins(JavaAppPackaging)

packageSummary in Linux := "compiler to generate binary data parsers in Java / Python / Ruby"
packageSummary in Windows := "Compiler for declarative YAML-based language to generate binary data parsers in Java / Python / Ruby"
packageDescription :=
"""This is the reference implementation of a compiler for Kaitai
Struct (.ksy) files. It allows to compile them into source code in
Java / Python / Ruby.

.ksy files describe binary data structures in declarative YAML-based
language (in contrast to imperative parsing implementation written in a
single programming language) and allow cross-language, cross-platform
data formats description."""

maintainer in Windows := "Kaitai Project"
maintainer in Debian := "Mikhail Yakshin <greycat@kaitai.io>"

mainClass in Compile := Some("io.kaitai.struct.Main")

lazy val compiler = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
    settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "io.kaitai.struct"
    )
