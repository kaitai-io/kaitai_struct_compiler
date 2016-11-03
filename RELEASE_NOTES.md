# 0.5

* Target languages support:
  * C++/STL - fully supported, all tests pass
  * Python - made compiled code and runtime compatible with both Python 2 and 3, enforced by CI
  * PHP7 - new target language, 98% supported
  * Perl - new target language, 85% supported
  * Graphviz - allows generation of visual diagrams of data formats, to be laid out with GraphViz (`.dot` format)
* New KSY language features:
  * Switch-like conditional structure to determine `type` based on value of expression (instead of tons of `if`s)
  * Attribute field `doc` to annotate fields - will generate docstrings relevant to language (i.e. JavaDoc, JSDoc, YARD/RDoc, etc)
  * `repeat-until` allows repetition of a field until a condition is met
  * Boolean type support
* Expression language:
  * `_io.eof` returns boolean value - whether the end of stream was reached or not
  * `_io.pos` returns current position in the stream
  * `_io.size` returns size of the stream
* .ksy parsing improvements:
  * New unified type derivation engine allows compile-time type error checks and full support of target languages which require absolute type designations (like C++, Python, Perl or PHP)
  * Same YAML parsing code is now used for both JVM and JS platforms
  * Stricter checks on all parsing stages: lots of invalid combinations are now prohibited (instead of choosing one of variants)
  * Better error messages: now in most cases compiler clearly indicates source of the problem
* Build and release process:
  * Compiler: added building as pom module
  * Java runtime: added building as pom module
  * Python runtime: added building as pip module
  * Windows CI: now all commits are built also on Windows, with [.msi package available for download](https://ci.appveyor.com/project/GreyCat/kaitai-struct/build/artifacts)
* Debug mode:
  * Support implemented for Java and JavaScript (to allow creation of visualizer tools in these languages - see [Java GUI for Kaitai Struct](https://github.com/kaitai-io/kaitai_struct_gui) and [Web IDE for Kaitai Struct](https://github.com/koczkatamas/kaitai_struct_webide)
  * Added generation of `SEQ_FIELDS` helper const array that allows clear separation of sequence attributes vs instance without guesswork
  * Exception in debug mode now tries to save as much parsed data as possible (to aid diagnosing the error)
* Incompatible changes:
  * Identifiers are now strictly checked to conform to `lower_underscore_case` pattern (that would be converted to language-specific style on ouput)
  * Java:
    * `_parse` method renamed to `_read`
    * `process*` methods are now static
  * JavaScript: `position` in runtime is renamed to `pos` (to conform to general KS API spec)
  * Compiler API: now all compilers accept unified `RuntimeConfig` for configuration instead of individual options
* Bugfixes:
  * Java:
    * having `if` on a sequence attribute now makes it automatically boxed (to allow it to be `null`)
    * work around some `int` vs `long` incompatibilities
    * proper boxing of floating types
  * Integer modulo (`%`) operation now behaves exactly the same in all languages, always returning positive result (as opposed to remainder operation `%` in languages like C++ or Java)

# 0.4 (2016-08-09)

* Languages support:
  * New target language, fully supported: C# (modules should be usable all across the .NET platform, i.e. from C++/CLI, VB.NET, F#, etc.)
  * Preliminary support for C++ (with STL containers / IO implementation) - note that not all features are implemented.
* Data types:
  * Floating point data types support (available as `f4` and `f8` for single and double precision IEEE754 floats)
  * Separate data type for byte arrays (including support for literal byte arrays)
* Expressions language:
  * Added new testing framework for expression translators
  * Added `.first` and `.last` for arrays (getting first and last element of array)
  * Added `.to_i` for strings (string -> int conversion)
  * Support for accessing `_io` object (IO stream) to access current stream's size (`_io.size`)
* Processing: extended "xor" processing to support XORing with multi-byte keys
* Runtime libraries:
  * Lots of cleanup - now all libraries try to follow the same strict standard (with method naming, parameters, order of methods, etc).
  * JavaScript: implemented full streaming API (both signed & unsigned integer, ensuring fixed contents fields, approximated 64-bit integers, etc).

# 0.3 (2016-04-21)

* New process: "ror/rol" (for simple circular bit shift)
* Ruby: runtime classes reside in a proper namespace: `Kaitai::Struct::Struct` and `Kaitai::Struct::Stream`, now just `KaitaiStruct` and `KaitaiStream`
* Scala.js build: fully implemented, now compiler can be called on a web page as a JavaScript library
* Implemented `process: ` for pre-processing input buffer of user types
* Translator: allow coercing of different int types into each other
* General code cleanup

# 0.2 (2016-04-06)

* Initial public release
