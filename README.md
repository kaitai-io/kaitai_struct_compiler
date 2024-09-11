# Kaitai Struct: compiler

[![Build Status](https://img.shields.io/github/actions/workflow/status/kaitai-io/kaitai_struct_compiler/test.yml?branch=master)](https://github.com/kaitai-io/kaitai_struct_compiler/actions/workflows/test.yml?query=branch%3Amaster)
[![Maven Central](https://img.shields.io/maven-central/v/io.kaitai/kaitai-struct-compiler_2.12)](https://search.maven.org/artifact/io.kaitai/kaitai-struct-compiler_2.12)

[![npm@latest](https://img.shields.io/npm/v/kaitai-struct-compiler/latest)](https://www.npmjs.com/package/kaitai-struct-compiler/v/latest)
[![npm@next](https://img.shields.io/npm/v/kaitai-struct-compiler/next)](https://www.npmjs.com/package/kaitai-struct-compiler/v/next)
[![npm downloads](https://img.shields.io/npm/dm/kaitai-struct-compiler?label=npm%20downloads)](https://www.npmtrends.com/kaitai-struct-compiler)

This project is an official reference compiler for [Kaitai Struct](http://kaitai.io) project.

Kaitai Struct is a declarative language used to describe various
binary data structures, laid out in files or in memory: i.e. binary
file formats, network stream packet formats, etc.

The main idea is that a particular format is described in Kaitai
Struct language (`.ksy` files) only once and then can be compiled with
this compiler into source files in one of the supported programming
languages. These modules will include the generated code for a parser
that can read described data structure from a file / stream and give
access to it in a nice, easy-to-comprehend API.

## Further information

If you're looking for information on:

* Kaitai Struct language itself (`.ksy` files, general usage patterns)
  — refer to the [user guide](http://doc.kaitai.io/user_guide.html).
* How to download and install Kaitai Struct — see the
  [downloads](http://kaitai.io/#download).
* How to build the compiler, run the test suite, and join the
  development — see the [developer memo](http://doc.kaitai.io/developers.html).

## Licensing

### Main code

Kaitai Struct compiler itself is copyright (C) 2015-2024 Kaitai
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

### XMLUtils code

Portions of Kaitai Struct compiler are based on `scala/xml/Utility.scala` from [Scala XML](https://github.com/scala/scala-xml).

Copyright (c) 2002-2017 EPFL\
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
* [fastparse](https://com-lihaoyi.github.io/fastparse/) — MIT license
* [snakeyaml](https://bitbucket.org/snakeyaml/snakeyaml) — Apache 2.0 license

---

Note that these clauses only apply only to compiler itself, not `.ksy`
input files that one supplies in normal process of compilation, nor to
compiler's output files — that consitutes normal usage process and you
obviously keep copyright to both.
