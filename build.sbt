import com.typesafe.sbt.packager.linux.LinuxSymlink
import sbt.Keys._

resolvers += Resolver.sonatypeRepo("public")

val VERSION = "0.7-SNAPSHOT"
val TARGET_LANGS = "C++/STL, C#, Java, JavaScript, Perl, PHP, Python, Ruby"

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
    version := VERSION,
    licenses := Seq(("GPL-3.0", url("https://opensource.org/licenses/GPL-3.0"))),
    scalaVersion := "2.11.7",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "io.kaitai.struct",
    buildInfoOptions += BuildInfoOption.BuildTime,

    // Repo publish options
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
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
      "com.lihaoyi" %%% "fastparse" % "0.4.1",
      "org.yaml" % "snakeyaml" % "1.16"
    )
  ).
  jvmSettings(
    mainClass in Compile := Some("io.kaitai.struct.Main"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "com.github.scopt" %% "scopt" % "3.4.0"
    ),

    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test_out"),

    // Create symlink to allow calling compiler quickly as "ksc"
    linuxPackageSymlinks += LinuxSymlink("/usr/bin/ksc", s"/usr/bin/${name.value}"),

    // Remove all "maintainer scripts", such as prerm/postrm/preinst/postinst: default
    // implementations create per-package virtual user that we won't use anyway
    maintainerScripts in Debian := Map(),

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

    wixProductLicense := Some(new File("shared/src/windows/License.rtf")),

    maintainer in Windows := "Kaitai Project",
    maintainer in Debian := "Mikhail Yakshin <greycat@kaitai.io>"
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val compilerJVM = compiler.jvm
lazy val compilerJS = compiler.js
