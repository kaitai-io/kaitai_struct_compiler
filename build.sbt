import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files

import com.typesafe.sbt.packager.linux.{LinuxPackageMapping, LinuxSymlink}
import sbt.Keys._

resolvers += Resolver.sonatypeRepo("public")

val NAME = "kaitai-struct-compiler"
val VERSION = "0.10-SNAPSHOT"
val TARGET_LANGS = "C++/STL, C#, Java, JavaScript, Lua, Nim, Perl, PHP, Python, Ruby"
val UTF8 = Charset.forName("UTF-8")

lazy val root = project.in(file(".")).
  aggregate(compilerJS, compilerJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val compiler = crossProject.in(file(".")).
  enablePlugins(JavaAppPackaging).
  settings(
    organization := "io.kaitai",
    version := sys.env.getOrElse("KAITAI_STRUCT_VERSION", VERSION),
    licenses := Seq(("GPL-3.0", url("https://opensource.org/licenses/GPL-3.0"))),
    scalaVersion := "2.12.12",

    // Repo publish options
    publishTo := version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }.value,
    pomExtra :=
      <url>http://kaitai.io</url>
      <scm>
        <connection>scm:git:git://github.com/kaitai-io/kaitai_struct_compiler.git</connection>
        <developerConnection>scm:git:ssh://github.com:kaitai-io/kaitai_struct_compiler.git</developerConnection>
        <url>http://github.com/kaitai-io/kaitai_struct_compiler/tree/master</url>
      </scm>
      <developers>
        <developer>
          <name>Mikhail Yakshin</name>
          <email>greycat.na.kor@gmail.com</email>
          <organization>Kaitai Project</organization>
          <organizationUrl>http://kaitai.io</organizationUrl>
        </developer>
      </developers>
    ,

    generateVersion := generateVersionTask.value, // register manual sbt command
    sourceGenerators in Compile += generateVersionTask.taskValue, // update automatically on every rebuild

    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "3.6.0",
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "org.yaml" % "snakeyaml" % "1.25"
    )
  ).
  jvmSettings(
    name := NAME,

    mainClass in Compile := Some("io.kaitai.struct.JavaMain"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),

    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test_out"),

    // Universal: add extra files (formats repo) for distribution, removing
    // .git special files and various dirty/backup files that git normally
    // ignores.
    //
    // NOTE: This requires formats repo to be checked out on the level higher
    // that the compiler
    mappings in Universal ++= NativePackagerHelper.directory("../formats").filterNot {
      case (_, dst) =>
        val dstFile = new File(dst)
        val dstFileName = dstFile.getName
        dst.startsWith(s"formats${File.separator}_") ||
          dstFileName == ".git" ||
          dstFileName.endsWith("~") ||
          dstFileName.endsWith("#")
    },

    // Uncomment if ever need to debug Windows file lists
//    mappings in Windows := (mappings in Universal).value.map { (x) =>
//      System.err.println("WINDOWS MAPPING: " + x)
//      x
//    },

    // Create symlink to allow calling compiler quickly as "ksc"
    linuxPackageSymlinks += LinuxSymlink("/usr/bin/ksc", s"/usr/bin/${name.value}"),

    // Add symlink for ksy files library location for Linux packages
    linuxPackageSymlinks += LinuxSymlink(s"/usr/share/${name.value}/formats", "/usr/share/kaitai-struct"),

    // Formats should be present in universal (for zips, etc), but
    // should be filtered out from Linux package, as we'll pack them
    // in separate package there
    linuxPackageMappings := {
      linuxPackageMappings.value.map { (lpm) =>
//        System.err.println("== mapping start")
        val r = lpm.copy(mappings = lpm.mappings.filterNot { case (src, dst) =>
//          System.err.println(s"DEBIAN MAP FILTER: $src -> $dst")
          val srcStr = src.toString
          srcStr == "../formats" || srcStr.startsWith("../formats/")
        })
//        System.err.println(s"== mapping stop: $r")
        r
      }
    },

    // We need /usr/share/kaitai-struct (the format directory) to be created as
    // empty dir and packaged in compiler package, to be filled in with actual
    // repository contents by "kaitai-struct-formats" package.
    linuxPackageMappings += packageTemplateMapping("/usr/share/kaitai-struct")(),

    // Remove all "maintainer scripts", such as prerm/postrm/preinst/postinst: default
    // implementations create per-package virtual user that we won't use anyway
    maintainerScripts in Debian := Map(),

    // Work around new Debian defaults and sbt-native-packager defaults, which
    // build .deb packages that appear to be incompatible with older Debian/Ubuntu's
    // dpkg and are not accepted by BinTray.
    //
    // For more information, see
    // https://github.com/sbt/sbt-native-packager/issues/1067
    debianNativeBuildOptions in Debian := Seq("-Zgzip", "-z3"),

    debianPackageDependencies := Seq("java8-runtime-headless"),

    packageSummary in Linux := s"compiler to generate binary data parsers in $TARGET_LANGS",
    packageSummary in Windows := "Kaitai Struct compiler",
    packageDescription in Linux :=
      s"""This is the reference implementation of a compiler for Kaitai Struct (.ksy)
       | files. It allows to compile them into source code in:
       | $TARGET_LANGS.
       | .
       | .ksy files describe binary data structures in declarative YAML-based
       | language (in contrast to imperative parsing implementation written in a
       | single programming language) and allow cross-language, cross-platform data
       | formats description.""".stripMargin,
    packageDescription in Windows := s"Compiler to translate Kaitai Struct (.ksy) files into $TARGET_LANGS source code",

    // Fix version for Windows: Wix doesn't allow stuff like "-SNAPSHOT" to appear in the version
    version in Windows := VERSION.replace("-SNAPSHOT", ""),

    wixProductId := "ddcf06bc-fd48-434b-93db-1e97ba8d13a7",
    wixProductUpgradeId := "63e85f5f-7680-4b3e-9bb9-dea0f70e970a",
    wixProductLicense := Some(new File("shared/src/windows/License.rtf")),

    maintainer in Windows := "Kaitai Project",
    maintainer in Debian := "Mikhail Yakshin <greycat@kaitai.io>"
  ).
  jsSettings(
    name := NAME + "-js",
    buildNpmJsFile := buildNpmJsFileTask.value,
    buildNpmPackage := buildNpmPackageTask.value
  )

lazy val compilerJVM = compiler.jvm
lazy val compilerJS = compiler.js

lazy val generateVersion = taskKey[Seq[File]]("generateVersion")
lazy val generateVersionTask = Def.task {
  // Generate contents of Version.scala
  val contents = s"""package io.kaitai.struct
                    |
                    |object Version {
                    |  val name = "${name.value}"
                    |  val version = "${version.value}"
                    |  val gitCommit = "${sys.env.getOrElse("GIT_COMMIT", "GIT_COMMIT not defined")}"
                    |  val gitTime = "${sys.env.getOrElse("GIT_DATE_ISO", "GIT_DATE_ISO not defined")}"
                    |}
                    |""".stripMargin

  // Update Version.scala file, if needed
  val file = (sourceManaged in Compile).value / "version" / "Version.scala"
  println(s"Version file generated: $file")
  IO.write(file, contents)
  Seq(file)
}

/**
  * Builds JavaScript output file to be packaged as part of NPM package.
  * Essentially wraps raw JavaScript output into AMD-style exports.
  */
lazy val buildNpmJsFile = taskKey[Seq[File]]("buildNpmJsFile")
lazy val buildNpmJsFileTask = Def.task {
  val compiledFile = target.value / "scala-2.12" / s"${name.value}-fastopt.js"
  println(s"buildNpmJsFile: reading $compiledFile")
  val compiledFileContents = IO.read(compiledFile, UTF8)

  val fileWithExports =
    s"""(function (root, factory) {
       |  if (typeof define === 'function' && define.amd) {
       |    define([], factory);
       |  } else if (typeof module === 'object' && module.exports) {
       |    module.exports = factory();
       |  } else {
       |    root.KaitaiStructCompiler = factory();
       |  }
       |}(this, function () {
       |
       |var exports = {};
       |var __ScalaJSEnv = { exportsNamespace: exports };
       |
       |$compiledFileContents
       |
       |return exports.io.kaitai.struct.MainJs;
       |
       |}));
     """.stripMargin

  val targetFile = new File(s"js/npm/${NAME}.js")
  println(s"buildNpmJsFile: writing $targetFile with AMD exports")
  IO.write(targetFile, fileWithExports, UTF8)
  Seq(targetFile)
}

lazy val buildNpmPackage = taskKey[Seq[File]]("buildNpmPackage")
lazy val buildNpmPackageTask = Def.task {
  val licenseFile = new File("js/npm/LICENSE")
  val readMeFile = new File("js/npm/README.md")
  val packageJsonFile = new File("js/npm/package.json")

  Files.copy(new File("LICENSE").toPath, licenseFile.toPath)
  Files.copy(new File("js/README.md").toPath, readMeFile.toPath)

  val packageJsonTmpl = IO.read(new File("js/package.json"), UTF8)
  val packageJsonContents = packageJsonTmpl.replaceFirst(
    "\"version\": \".*?\"",
    "\"version\": \"" + version.value.replace("-SNAPSHOT", ".0-SNAPSHOT") + "\""
  )

  IO.write(packageJsonFile, packageJsonContents, UTF8)

  Seq(
    licenseFile,
    readMeFile,
    packageJsonFile
  )
}
