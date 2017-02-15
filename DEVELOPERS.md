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

### Building an [universal] (.zip) package

[universal]: http://www.scala-sbt.org/sbt-native-packager/formats/universal.html

`sbt compilerJVM/universal:packageBin` → generates `jvm/target/universal/kaitai-struct-compiler-*.zip`

### Building [Debian package]

[Debian package]: http://www.scala-sbt.org/sbt-native-packager/formats/debian.html

1. Install prerequisites: `sudo -i apt-get install dpkg-deb dpkg-sig dpkg-genchanges lintian fakeroot`
2. `sbt compilerJVM/debian:packageBin` -> generates `jvm/target/kaitai-struct-compiler_*_all.deb`

### Building [Windows package]

[Windows package]: http://www.scala-sbt.org/sbt-native-packager/formats/windows.html

1. Install WIX
2. `sbt compilerJVM/windows:packageBin` -> genereates `jvm/target/windows/kaitai-struct-compiler.msi`
3. Rename to add version to `kaitai-struct-compiler-$VERSION.msi`

### Dependencies for JVM target

TODO

## Building for JavaScript platform

Building to JavaScript platform is done using a Scala.js project. Note
that it uses a somewhat different set of dependencies, as they must
actually be JavaScript libraries, not Java jars.

1. Run `sbt fastOptJS` -> generates `js/target/scala-2.11/kaitai-struct-compiler-fastopt.js`
2. Use this JavaScript file on a website

### Dependencies for JavaScript target

TODO

## Publishing a new version

1. Choose a new version number (WIX imposes harsh requirements for
  version to look like `x.x.x.x`) and update it in `build.sbt`,
  `version := ...`, commit
2. Prepare an entry in RELEASE_NOTES.md, commit
3. Create version tag:
  * `git tag $VERSION`
  * `git push --tags`
4. Update [main repository](https://github.com/kaitai-io/kaitai_struct)
5. Create new version at:
  * https://bintray.com/kaitai-io/debian/kaitai-struct-compiler/new/version
  * https://bintray.com/kaitai-io/universal/kaitai-struct-compiler/new/version
6. Upload:
  * https://bintray.com/kaitai-io/debian/kaitai-struct-compiler/$VERSION/upload
    * Debian distribution: `jessie`
    * Debian component: `main`
    * Debian architecture: `all`
    * Attached file: `jvm/target/kaitai-struct-compiler_*_all.deb`
  * https://bintray.com/kaitai-io/universal/kaitai-struct-compiler/$VERSION/upload
    * Target path: `$VERSION`
    * Attached file: `jvm/target/universal/kaitai-struct-compiler-*.zip`
  * https://bintray.com/kaitai-io/universal/kaitai-struct-compiler/$VERSION/upload
    * Target path: `$VERSION`
    * Attached file: `jvm/target/windows/kaitai-struct-compiler-*.msi`
7. Publish them all

### Runtimes

#### Java

* Pump version, set version to `$VERSION`, without `-SNAPSHOT`
* `mvn deploy`
* Go to https://oss.sonatype.org/#stagingRepositories
* Scroll to the very end of list, seek `iokaitai-...` repositories
* Select our staging repository
* Press "Close" toolbar button
  * Confirm
  * Wait for checks to complete
* Press "Release" toolbar button
  * Enter release message
  * Confirm
* After some time, check https://search.maven.org/#search%7Cga%7C1%7Ca%3A%22kaitai-struct-runtime%22 to have new version

#### Python

* Pump version in `setup.py`, seek `version=`
* `python3 setup.py sdist upload`
  * (use `python3 setup.py sdist upload -r pypitest` to publish to testing server)
* Check that new version appears at https://pypi.python.org/pypi/kaitaistruct/$VERSION
* `git tag $VERSION`
* `git push --tags`

#### Ruby

* Pump version in `lib/kaitai/struct/struct.rb`, seek `VERSION = `
* `gem build kaitai-struct.gemspec`
* Test gem (i.e. by installing it to a live system)
* `gem push kaitai-struct-$VERSION.gem`
* `git tag $VERSION`
* `git push --tags`

## Adding new language

Don't forget to update lists of languages:

* /build.sbt - supportedLanguages
* https://github.com/kaitai-io/kaitai_struct — project description
* https://github.com/kaitai-io/kaitai_struct_compiler — project description
* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/README.md — `-t` option documentation
* http://kaitai.io — everywhere
* https://bintray.com/kaitai-io/debian/kaitai-struct-compiler/view — package description
* https://twitter.com/kaitai_io — profile
