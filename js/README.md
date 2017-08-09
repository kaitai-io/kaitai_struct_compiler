## Kaitai Struct compiler in JS

* TODO: a few brief introduction: what it is, what it does, pointers to main site

* TODO: explanation of the fact it's written in Scala and the build process

* TODO: where it is to be used (i.e. that you don't really need to depend on this component if you're just a regular user and not developer of KS-based tools)

## Installation and plugging in

### Installation

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

### Plugging in

We publish the compiler as an [UMD module](https://github.com/umdjs/umd), so it works from various environments, including server-side (eg. node) and client-side (eg. web browser) ones.

Note: currently we don't publish the compiler as standard ES module. This will probably change in the future. If you need ES module please comment on [this issue](https://github.com/kaitai-io/kaitai_struct/issues/180).

#### node

```javascript
var KaitaiStructCompiler = require("kaitai-struct-compiler");
var compiler = new KaitaiStructCompiler();
```

#### browser using script tags

```html
<script src="node_modules/kaitai-struct-compiler/kaitai-struct-compiler.js"></script> 
<script>
    var compiler = new KaitaiStructCompiler();
</script>
```

#### browser using AMD loader (eg. require.js)

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/require.js/2.3.4/require.min.js"></script>
<script>
    requirejs.config({ 
        paths: {
            'kaitai-struct-compiler': 'node_modules/kaitai-struct-compiler/kaitai-struct-compiler'
        }
    });

    require(['kaitai-struct-compiler'], function(KaitaiStructCompiler){
        var compiler = new KaitaiStructCompiler();
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

You can compile in debug mode which adds `_debug` property to every object and this `_debug` object contains the start and end offsets of the parsed fields so you can find bytes of the field in the original binary.

## Copyrights and licensing

Kaitai Struct compiler itself is copyright (C) 2015-2017 Kaitai
Project.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Note that it applies only to compiler itself, not `.ksy` input files
that one supplies in normal process of compilation, nor to compiler's
output files — that consitutes normal usage process and you obviously
keep copyright to both.
