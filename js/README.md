## Kaitai Struct compiler in JavaScript

This project is a official reference Kaitai Struct compiler, compiled
for JavaScript environments.

Kaitai Struct is a declarative language used for describe various
binary data structures, laid out in files or in memory: i.e. binary
file formats, network stream packet formats, etc.

The main idea is that a particular format is described in Kaitai
Struct language only once and then can be compiled with  into
source files in one of the supported programming languages. These
modules will include a generated code for a parser that can read
described data structure from a file / stream and give access to it in
a nice, easy-to-comprehend API.

For more info on Kaitai Struct, please refer to http://kaitai.io/

Note that reference Kaitai Struct compiler is written in Scala, and thus
can be compiled for a variety of platforms, such as JVM, JavaScript
and native binaries. This package is compiled to be run JavaScript
environments.

Currently, this JavaScript build offers only programmatic API, so it
is generally suited for developers of tools that use Kaitai Struct
(i.e. interactive compilers, visualizers, loaders, IDEs that integrate
the compiler). If you:

* just look for a way to try out Kaitai Struct => try
  [Kaitai Struct Web IDE](https://ide.kaitai.io/), which uses this
  compiler internally
* want to load .ksy file transparently in your JavaScript code
  (compiling it on the fly) => use
  [Kaitai Struct loader for JavaScript](https://github.com/kaitai-io/kaitai-struct-loader).
* want to integrate compiler into your build flow => use a JVM-based
  [desktop compiler](https://kaitai.io/#download), which can be called
  as a command-line utility

## Installation

We publish two versions of the compiler to npm:
 - A stable one, this includes the latest stable, released compiler. This is the default ("latest") version.
   ```
   npm install kaitai-struct-compiler
   ```
 - The other is the latest snapshot version which follows our master branch. This version is tagged as `@next`.
   ```
   npm install kaitai-struct-compiler@next
   ```

### Example project

Our [examples repository](https://github.com/kaitai-io/kaitai_struct_examples) contains a few examples how to use the compiler.

## Plugging in

> **Note:** Before version 0.11.0, the `kaitai-struct-compiler` module returned a _compiler constructor_ (called `KaitaiStructCompiler`) and you would use it as `(new KaitaiStructCompiler()).compile(...)`, but since 0.11.0 it returns the _compiler object_ itself and you just need to do `KaitaiStructCompiler.compile(...)`. Make sure to adapt your code.

We publish the compiler as a [UMD module](https://github.com/umdjs/umd), so it works from various environments, including server-side (e.g. Node.js) and client-side (e.g. web browser) ones.

Note: currently we don't publish the compiler as standard ES module. This will probably change in the future. If you need ES module, please comment on [this issue](https://github.com/kaitai-io/kaitai_struct/issues/180).

### node

```javascript
var compiler = require("kaitai-struct-compiler");
```

### browser using script tags

```html
<script src="node_modules/kaitai-struct-compiler/kaitai-struct-compiler.js"></script>
<script>
    var compiler = KaitaiStructCompiler;
</script>
```

### browser using AMD loader (e.g. require.js)

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/require.js/2.3.4/require.min.js"></script>
<script>
    requirejs.config({
        paths: {
            'kaitai-struct-compiler': 'node_modules/kaitai-struct-compiler/kaitai-struct-compiler'
        }
    });

    require(['kaitai-struct-compiler'], function(compiler){
        // ...
    });
</script>
```

## Usage

### Basic usage of compile method

```javascript
var ksyYaml = fs.readFileSync("zip.ksy"); /* string */
var ksy = YAML.parse(ksyYaml); /* JS object */
compiler.compile("javascript", ksy, null, false /* debugMode */).then(function(files) {
    console.log("Compiled filenames: " + Object.keys(files).join(", "));
    console.log("Content of Zip.js file: " + files["Zip.js"]);
});
```

### Getting compiler information

```javascript
console.log("Version: " + compiler.version);
console.log("Build date: " + compiler.buildDate);
console.log("Supported languages: " + compiler.languages.join(", "));
```

### Handle imports

```javascript
var yamlImporter = {
    importYaml: function(name, mode) {
        console.log("  -> Import yaml called with name '" + name + "' and mode '" + mode + "'.");
        var importKsyYaml = fs.readFileSync(name + ".ksy"); /* string */
        var importKsy = YAML.parse(importKsyYaml); /* JS object */
        return Promise.resolve(importKsy);
    }
};

yamlImporter.importYaml("import_outer.ksy").then(function(ksy) {
  compiler.compile("javascript", ksy, yamlImporter, false /* debugMode */).then(function(files) {
      console.log("Compiled filenames: " + Object.keys(files).join(", "));
  });
});
```

### Debug mode

You can compile `.ksy` source files in debug mode by setting the `debugMode` parameter to `true`. This has two effects (these can be controlled separately in the JVM-based desktop compiler since 0.9 - see [#332](https://github.com/kaitai-io/kaitai_struct/issues/332#issuecomment-454795155), but not yet in this JS compiler):

* objects created from user-defined `types` (and the top-level type with name specified in `/meta/id`) in the .ksy file will have a `_debug` map, which stores start and end offsets of each attribute - this is used in visualizers.

  On the JVM-based desktop compiler, this can be enabled with `--read-pos`.

* the `_read` method (responsible for reading the `seq` structure defined in the .ksy file) will no longer be automatically called from constructors in the generated parser code, so you have to call it manually like this:

  ```diff
   var g = new Gif(new KaitaiStream(data));
  +g._read();
  ```

  On the JVM-based desktop compiler, this can be enabled with `--no-auto-read`.

## Licensing

### Main code

Kaitai Struct compiler itself is copyright (C) 2015-2022 Kaitai
Project.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

### FastParse

Portions of Kaitai Struct compiler are loosely based on
[pythonparse](https://github.com/com-lihaoyi/fastparse/tree/1.0.0/pythonparse/shared/src/main/scala/pythonparse)
from [FastParse](https://com-lihaoyi.github.io/fastparse/) and are copyright
(c) 2014 Li Haoyi (haoyi.sg@gmail.com).

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Libraries used

Kaitai Struct compiler in JavaScript depends on the following libraries:

* [fastparse](https://com-lihaoyi.github.io/fastparse/) — MIT license

---

Note that these clauses only apply only to compiler itself, not `.ksy`
input files that one supplies in normal process of compilation, nor to
compiler's output files — that constitutes normal usage process and you
obviously keep copyright to both.
