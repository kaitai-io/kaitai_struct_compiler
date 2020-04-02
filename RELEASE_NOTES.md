# 0.9 (TBD)

* New targets support:
  * Python with [Construct library](https://construct.readthedocs.io)
* Expression language:
  * New methods:
    * byte arrays: `length`
* General compilation improvements:
  * Support Maven-like directory trees by not adding subdir `src` for outputs of Go+Java anymore,
    see [#287](https://github.com/kaitai-io/kaitai_struct/issues/287). While this breaks existing
    builds most likely, it puts those languages in line with all others and adding subdirs is easier
    for the user than removing some added by Kaitai automatically.

# 0.8 (2018-02-05)

* New target languages:
  * Lua (96% tests pass score)
  * initial support for Go (15% tests pass score)
* New ksy features:
  * Switchable default endianness: `meta/endian` can now contain a
    switch-like structure (with `switch-on` and `cases`), akin to
    switchable types
    ([docs](http://doc.kaitai.io/user_guide.html#calc-endian)).
  * Parametric user-defined types: one can use `type: my_type(arg1,
    arg2, arg3)` to pass arguments into user type
    ([docs](http://doc.kaitai.io/user_guide.html#param-types)).
  * Custom processing types: one can use `process:
    my_process_name(arg1, arg2, arg3)` to invoke custom processing
    routine, implemented in imperative language
    ([docs](http://doc.kaitai.io/user_guide.html#custom-process)).
  * In repetitions, index of current repetition can be accessed using
    `_index` in expressions
    ([docs](http://doc.kaitai.io/user_guide.html#repeat-index)).
  * Verbose enums: now one can specify documentation and other useful
    information relevant to enums using verbose enum declaration
    format
    ([docs](http://doc.kaitai.io/user_guide.html#verbose-enums)).
  * `meta/xref` key can be used for adding cross-references of a
    format specifications (like relevant RFC entries, Wikidata
    entries, ISO / IEEE / JIS / DIN / GOST standard numbers, PRONOM
    identifiers, etc).
* General compilation improvements:
  * Imports/includes for all languages are now managed properly, no
    duplicate / unnecessary imports should be added
  * Python: basic docstring support
  * More strict ksy precompile checks (less likely to accept ksy that
    will result in non-compilable code), better error messages
* CLI options:
  * Python target now allows to specify package with `--python-package`
  * Java target now allows custom KaitaiStream implementations and
    thus allows to specify default implementation for `fromFile(...)`
    using `--java-from-file-class`.
* Expression language:
  * New methods:
    * floats: `to_i`
    * arrays: `min`, `max`
  * Added byte array comparison
* Packaging / infrastructure improvements:
  * ksc is now available as
    [npm package](https://www.npmjs.com/package/kaitai-struct-compiler/),
    which now a build dependency of a
    [web IDE](https://ide.kaitai.io/)
* Runtime API changes:
  * C++: now requires `KS_STR_ENCODING_ICONV` or
    `KS_STR_ENCODING_NONE` to be defined to how to handle string
    encodings
  * Java: `KaitaiStream` is now an interface, and there are two
    distinct classes which implement it:
    * `ByteBufferKaitaiStream` provides KaitaiStream backed
      `ByteBuffer` (and thus using memory-mapped files)
    * `RandomAccessFileKaitaiStream` provides KaitaiStream backed by
      `RandomAccessFile` (and thus uses normal OS read calls, as it
      was done in older KaitaiStruct circa v0.5)
  * JavaScript: Error classes are now subclasses of `KaitaiStream` and
    were renamed in the following way: `KaitaiUnexpectedDataError` ->
    `KaitaiStream`.`UnexpectedDataError`
* Major bugfixes:
  * C++: adjusted to made compatible with OS X and Windows MSVC builds
  * Fixed broken generation of byte array literals with high 8-bit set
    in some targets
  * Fixed float literals parsing, fixed larger integer keys YAML parsing
  * Fixed inconsistency of debug mode vs non-debug mode behavior for
    `repeat-*`
  * Fixed chain of relative imports bug: now all relative imports work
    always relative to the file being processed, not to current
    compiler's dir
  * Many problems with switching: invalid common type inferring,
    invalid code being generated, added failsafe `if`-based
    implementations for languages which do not support switching over
    all possible types.
  * Fixed most memory leaks in C++ (only exception-related leaks are
    left now)

# 0.7 (2017-03-22)

* New ksy features:
  * Type importing system: `meta/import` can be used to import other
    types as first-class citizens in current compilation unit; "opaque
    types" are now disabled by default (see below)
  * Byte-terminated notation (`terminator`, `include` and `consume`)
    can be now used not only for strings, but also for any byte types
    and user types
  * `pad-right` to remove declare excess right padding (usually with
    0s)
  * User types can now use `parent: expression` to enforce a specific
    parent for an object, or `parent: false` to disable parenting at
    all (and, subsequently, remove it from parent type inferring
    process)
  * Type inferring: value instances are now allowed to use `_parent`
  * `doc-ref` to add references to external documentation for types /
    attributes
* Improved compilation process:
  * Compilation is now clearly separated in 3 phases: YAML parsing,
    precompilation, compilation. Phases 1 and 2 are language-agnostic
    and "precompilation" now does all possible sanity checks
    preliminary, making sure that language-specific "compilation"
    doesn't have to deal with invalid data.
  * Improved compilation results reporting: now all error messages
    reported by compiler have file / code location and proper
    user-readable text. Added more than 50 tests for erroneous input
    files. Exceptions thrown directly are considered a compiler bug
    from now on.
  * Generated code now checks for runtime library version
    compatibility and fails to compile / run with non-compliant
    runtime
* Command-line compiler options:
  * `--opaque-types=true` to enable opaque types (disabled by default,
    i.e. using unknown type would be treated as error)
  * `--verbose` now allows fine-tuned verbose logging for various
    compiler's subsystems; using `--verbose=all` exposes a lot of
    internal logic.
  * `--ksc-json-output` to dump compilation results in
    machine-readable JSON format (simplifies ksc integration in other
    tools, like visualizers)
* Console visualizer: faster loading, automatic handling of imports
  (no more need to specify all .ksy files manually on invocation)
* Expression language:
  * Two string types: single quotes (verbatim), double quotes
    (interpolating with escape characters)
  * New type casting operator: `.as<foo>`
  * New methods:
    * arrays: `size`
    * booleans: `to_i`
    * byte arrays: `to_s(encoding)`
    * enums: `to_i`
    * strings: `reverse`
* Runtime API changes:
  * All bytearray to string functions are named `bytes_to_str` in all
    languages
  * Added `read_bytes_term` (akin to what `read_str_term` did
    previously to strings)
  * Removed `read_str_*` methods, they are to be replaced now with
    combination of `read_bytes_*` + `bytes_to_str`
  * Added `bytes_strip_right` and `bytes_terminate`
  * Perl module now uses `IO::KaitaiStruct` package name (instead of
    `Kaitai`)
* Major bugfixes:
  * Recursive top-level types
  * Unaligned bits reading with enums on top of bit-level integers
  * `repeat-until` handling with substreams

# 0.6 (2017-02-04)

* Unaligned bit parsing support
  * Use `type: b12` to parse 12 bits as integer from a stream (obviously, one can use `b1`, `b2`, `b3`, etc)
  * `b1` is parsed as a boolean value
  * If several `bXX` are chained in a sequence, can be used to parse bit masks/fields
  * Using of regular types (i.e. `u1`, `s4`, `str`, etc) starts parsing normally, aligning to next byte
* More meta information, documentation and non-standard keys usage:
  * `doc` for docstrings is allowed on type level
  * `meta` can now include:
    * `title` (to give proper full title for type)
    * `license` (to specify work licensing)
    * `ks-version` (to specify minimal version of Kaitai Struct compiler that must be used to process a .ksy - i.e. `0.6`)
    * `ks-debug` (to enforce generation of classes as if `--debug` mode was specified in command line)
  * `meta` is non-global now, but can be used on multiple levels and inherited from closest one
  * Non-(yet)-standard keys can be used everywhere now using `-key` syntax: for example, Web IDE uses `-webide-representation` key which is ignored by the compiler, but useful for clearer debugging
* Enums are proper first-class citizens now: `enum: XXX` specifications are not just strings, but proper references to declared enums, thus they're checked for validity, can reference upper level nested enums from lower levels, etc - this fixes majority of existing enum namespacing problems in JavaScript, Python, PHP and Perl
* `id` in `seq` elements in now optional: it can be useful for quick exploration mapping (one can always assign identifiers later), or for unused ("reserved for later use") attributes - such attributes would be assigned numbered IDs automatically
* Allow value instances to use `if` and `enum`
* Proper support for "opaque" external types: one can use an undeclared data type, it's expected to be declared in some other .ksy file and it will be properly imported/included in current file
* Expression language:
  * Support for integer literals with underscores for readability: one can use stuff like `123_456_789` or `0b0101_0011` now
  * `to_s` method for integer types to convert them to strings
* Language-specific improvements:
  * C++: clearly separated "null" (no result, for example, due to failed `if` condition) and "not yet calculated" results - introduced `_is_null_XXX()` method for check for true null result in generated API
  * JavaScript: generated enums can be queried for both ID => name and name => ID
  * PHP: dropped type generation for now due to nullable types - one day they might return strictly for PHP 7.1+
  * GraphViz: major compatibility fixes, diagram readability improvements, support for switch types
* Runtime API changes:
  * `ensure_fixed_contents` no longer requires both expected byte array and its length, only array is required
  * Java: all methods no longer use checked exceptions, i.e. `IOException`
* Bugfixes:
  * Type derivation of parent types when using switched `type`, array types, and type combining on switching / ternary operators
  * Multiple translator fixes: type derivation, parenthesis generation
  * Assorted code generation bugfixes in C++, Python, Ruby
* Refactorings and optimizations:
  * Type derivation engine
  * Parse instances use more optimal order of conditionals / debug / IO management applications
  * Improved error messages

# 0.5 (2016-11-09)

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
