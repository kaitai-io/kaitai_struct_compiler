import java.io.File

import com.typesafe.sbt.packager.linux.{LinuxPackageMapping, LinuxSymlink}
import sbt.Keys._

resolvers += Resolver.sonatypeRepo("public")

val VERSION = "0.9-SNAPSHOT"
val TARGET_LANGS = "C++/STL, C#, Java, JavaScript, Lua, Perl, PHP, Python, Ruby"

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
    organization := "io.kaitai",
    name := "kaitai-struct-compiler",
    version := sys.env.getOrElse("KAITAI_STRUCT_VERSION", VERSION),
    licenses := Seq(("GPL-3.0", url("https://opensource.org/licenses/GPL-3.0"))),
    scalaVersion := "2.12.4",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "io.kaitai.struct",
    buildInfoOptions += BuildInfoOption.BuildTime,

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

    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "3.6.0",
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "org.yaml" % "snakeyaml" % "1.16"
    )
  ).
  jvmSettings(
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
    // Add JS-specific settings here
  )

lazy val compilerJVM = compiler.jvm
lazy val compilerJS = compiler.js
