name := "kaitai_structures_scala"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.16",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.1.1",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.1.1",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.1",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.1.1"
)
