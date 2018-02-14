### Submitting bugs / issues / ideas

> We're phasing out "Issues" tab in this repository. Please submit all issues to the [main project's tracker](https://github.com/kaitai-io/kaitai_struct/issues).

---

# Kaitai Struct: compiler

This project is an official reference compiler for [Kaitai Struct](https://github.com/kaitai-io/kaitai_struct) project.

Kaitai Struct is a declarative language used to describe various
binary data structures, laid out in files or in memory: i.e. binary
file formats, network stream packet formats, etc.

The main idea is that a particular format is described in Kaitai
Struct language (`.ksy` files) only once and then can be compiled with
this compiler into source files in one of the supported programming
languages. These modules will include the generated code for a parser
that can read described data structure from a file / stream and give
access to it in a nice, easy-to-comprehend API.

Please refer to [documentation in Kaitai Struct project](https://github.com/kaitai-io/kaitai_struct)
for details on `.ksy` files and general usage patterns.

## Trying without install

Kaitai Struct compiler can be tried instantly, without any downloads
and installation, at

http://kaitai.io/repl

Note that this implementation uses the same reference code as in this
repository and executes totally on a client side, without any queries
to server backend.

## Downloading and installing

### Linux .deb builds (Debian/Ubuntu)

There is an official .deb repository available. The repository is hosted
at BinTray and signed with BinTray GPG key (`379CE192D401AB61`), so it's
necessary to import that key first if your box haven't used any BinTray
repositories beforehand:

```shell
echo "deb https://dl.bintray.com/kaitai-io/debian jessie main" | sudo tee /etc/apt/sources.list.d/kaitai.list
sudo apt-key adv --keyserver hkp://pool.sks-keyservers.net --recv 379CE192D401AB61
sudo apt-get update
sudo apt-get install kaitai-struct-compiler
```

### Windows builds

An official `.msi` installer build is available to download at

https://bintray.com/kaitai-io/universal/kaitai-struct-compiler/_latestVersion

### Universal builds

Basically, everything that can run Java can use so called "universal"
builds: a .zip file that includes all the required .jar files bundled
and launcher scripts for UNIX/Windows systems. No installation
required, one can just unpack and run it. Available at download also at

https://bintray.com/kaitai-io/universal/kaitai-struct-compiler/_latestVersion

### Source code

If you're interested in developing compiler itself, you can check out
source code in repository:

    git clone https://github.com/kaitai-io/kaitai_struct_compiler

See the [developer documentation](http://doc.kaitai.io/developers.html) for 
general pointers on how to proceed with the source code then.

## Usage

`kaitai-struct-compiler [options] <file>...`

Alternatively, a symlink `ksc` is provided and can be used everywhere
just as full name.

Common options:

* `<file>...` — source files (.ksy)
* `-t <language> | --target <language>` — target languages (`graphviz`, `csharp`,
    `all`, `perl`, `java`, `go`, `cpp_stl`, `php`, `lua`, `python`, `ruby`, `javascript`
  * `all` is a special case: it compiles all possible target
    languages, creating language-specific directories (as per language
    identifiers) inside output directory, and then creating output
    module(s) for each language starting from there
* `-d <directory> | --outdir <directory>` — output directory
  (filenames will be auto-generated)

Language-specific options:

* `--dot-net-namespace <namespace>` — .NET namespace (C# only, default: Kaitai)
* `--java-package <package>` — Java package (Java only, default: root package)
* `--php-namespace <namespace>` — PHP namespace (PHP only, default: root package)

Misc options:

* `--verbose` — verbose output
* `--help` — display usage information and exit
* `--version` — output version information and exit

A few examples, given that file `foo.ksy` exists in current directory
and describes format with ID `foo`:

* `kaitai-struct-compiler -t python foo.ksy` — compile format in
  `foo.ksy`, write output in current directory to file `foo.py`
* `kaitai-struct-compiler -t java foo.ksy` — compile format in
  `foo.ksy`, create "src" subdir in current one and write output in
  `src/Foo.java`
* `kaitai-struct-compiler -t java --java-package org.example foo.ksy`
  — compile format in `foo.ksy`, create "src/org/example" subdir tree
  in current one and write output in `src/org/example/Foo.java`;
  resulting file will bear correct Java package clause.
* `kaitai-struct-compiler -t all -d /tmp/out --java-package org.example foo.ksy`
  — compile format in `foo.ksy`, creating a hierarchy of files:
  * `/tmp/out/java/src/org/example/Foo.java`
  * `/tmp/out/python/foo.py`
  * `/tmp/out/ruby/foo.rb`

## Licensing

### Main code

Kaitai Struct compiler itself is copyright (C) 2015-2018 Kaitai
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

### FastParse

Portions of Kaitai Struct compiler are loosely based on
[pythonparse](https://github.com/lihaoyi/fastparse/tree/master/pythonparse/shared/src/main/scala/pythonparse)
from [FastParse](http://www.lihaoyi.com/fastparse/) and are copyright
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

### XMLUtils code

Portions of Kaitai Struct compiler are based on `scala/xml/Utility.scala` from [Scala XML](https://github.com/scala/scala-xml).

Copyright (c) 2002-2017 EPFL
Copyright (c) 2011-2017 Lightbend, Inc.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the EPFL nor the names of its contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
THE POSSIBILITY OF SUCH DAMAGE.

### Libraries used

Kaitai Struct compiler depends on the following libraries:

* [scopt](https://github.com/scopt/scopt) — MIT license
* [fastparse](http://www.lihaoyi.com/fastparse/) — MIT license
* [snakeyaml](https://bitbucket.org/asomov/snakeyaml) — Apache 2.0 license

---

Note that these clauses only apply only to compiler itself, not `.ksy`
input files that one supplies in normal process of compilation, nor to
compiler's output files — that consitutes normal usage process and you
obviously keep copyright to both.
