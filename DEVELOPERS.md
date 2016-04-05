# Developers README

KS compiler is written in [Scala language] and thus uses [SBT] for
building. It can be compiled in one of 2 ways:

* generating Java `.class` files, to be run in JVM
* generating JavaScript `.js` file, to be run either inside a browser or
  in node.js environment

[Scala language]: http://www.scala-lang.org/
[SBT]: http://www.scala-sbt.org/

## Building for JVM

We use [sbt-native-packager] to build deployable formats.

[sbt-native-packager]: http://www.scala-sbt.org/sbt-native-packager/

### Building Debian package

http://www.scala-sbt.org/sbt-native-packager/formats/debian.html

1. Install prerequisites: `sudo -i apt-get install dpkg-deb dpkg-sig dpkg-genchanges lintian fakeroot`
2. `sbt compilerJVM/debian:packageBin` -> generates `jvm/target/kaitai-struct-compiler_*_all.deb`

TODO: document deployment to bintray

### Building Windows package

http://www.scala-sbt.org/sbt-native-packager/formats/windows.html

1. Install WIX
2. `sbt windows:packageBin`

### Dependencies for JVM target

## Building for JavaScript platform

Building to JavaScript platform is done using a Scala.js project. Note
that it uses a somewhat different set of dependencies, as they must
actually be JavaScript libraries, not Java jars.

1. Run `sbt fastOptJS` -> generates `js/target/scala-2.11/kaitai-struct-compiler-fastopt.js`
2. Use this JavaScript file on a website

### Dependencies for JavaScript target

## Adding new language

Don't forget to update lists of languages:

* https://github.com/kaitai-io/kaitai_struct_compiler — project description
* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/README.md — `-t` option documentation
