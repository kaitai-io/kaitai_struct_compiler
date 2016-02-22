# Kaitai Struct: compiler

This project is an official reference compiler for [Kaitai Struct](https://github.com/kaitai-io/kaitai_struct) project.

Kaitai Struct is a declarative language used for describe various
binary data structures, laid out in files or in memory: i.e. binary
file formats, network stream packet formats, etc.

The main idea is that a particular format is described in Kaitai
Struct language (`.ksy` files) only once and then can be compiled with
this compiler into source files in one of the supported programming
languages. These modules will include a generated code for a parser
that can read described data structure from a file / stream and give
access to it in a nice, easy-to-comprehend API.

Please refer to [documentation in Kaitai Struct project](https://github.com/kaitai-io/kaitai_struct)
for details on `.ksy` files and general usage patterns.

## Downloading and installing

### Linux .deb builds (Debian/Ubuntu)

There is an official .deb repository available.

echo "deb https://dl.bintray.com/kaitai-io/debian /" | sudo tee -a /etc/apt/sources.list.d/kaitai.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
sudo apt-get update
sudo apt-get install kaitai-struct-compiler

### Windows builds

TODO

### Source code

If you're interested in developing compiler itself, you can check out
source code in repository:

    git clone https://github.com/kaitai-io/kaitai_struct_compiler

See "Development" section below for general pointers on how to proceed
with the source code then.

## Usage

## Development

KS compiler itself is written in [Scala language](http://www.scala-lang.org/).

## Licensing

Kaitai Struct compiler itself is copyright (C) 2015-2016 Kaitai
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
