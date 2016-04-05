import com.typesafe.sbt.packager.linux.LinuxSymlink

name := "kaitai-struct-compiler"

resolvers += Resolver.sonatypeRepo("public")

lazy val root = project.in(file(".")).
  aggregate(compilerJS, compilerJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val compiler = crossProject.in(file(".")).
  enablePlugins(BuildInfoPlugin).
  enablePlugins(JavaAppPackaging).
  settings(
    name := "kaitai-struct-compiler",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "io.kaitai.struct",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fastparse" % "0.3.7",
      "org.yaml" % "snakeyaml" % "1.16",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.1.1",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.1.1",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.1",
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.1.1"
    )
  ).
  jvmSettings(
    mainClass in Compile := Some("io.kaitai.struct.Main"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "com.github.scopt" %% "scopt" % "3.4.0"
    ),

    linuxPackageSymlinks += LinuxSymlink("/usr/bin/ksc", s"/usr/bin/${name.value}"),

    packageSummary in Linux := "compiler to generate binary data parsers in Java / JavaScript / Python / Ruby",
    packageSummary in Windows := "Compiler for declarative YAML-based language to generate binary data parsers in Java / JavaScript / Python / Ruby",
    packageDescription :=
    """This is the reference implementation of a compiler for Kaitai Struct (.ksy)
    files. It allows to compile them into source code in Java / JavaScript /
    Python / Ruby.
    .ksy files describe binary data structures in declarative YAML-based
    language (in contrast to imperative parsing implementation written in a
    single programming language) and allow cross-language, cross-platform data
    formats description.""",

    maintainer in Windows := "Kaitai Project",
    maintainer in Debian := "Mikhail Yakshin <greycat@kaitai.io>"
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val compilerJVM = compiler.jvm
lazy val compilerJS = compiler.js
