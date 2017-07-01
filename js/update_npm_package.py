#!/usr/bin/env python3

import shutil

moduleTemplate = '''
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define([], factory);
  } else if (typeof module === 'object' && module.exports) {
    module.exports = factory();
  } else {
    root.KaitaiStructCompiler = factory();
  }
}(this, function () {

var exports = {};
var __ScalaJSEnv = { exportsNamespace: exports };

{{compilerCode}}

return exports.io.kaitai.struct.MainJs;

}));
'''.lstrip()

with open('target/scala-2.11/kaitai-struct-compiler-fastopt.js','rt') as f: compilerCode = f.read()

moduleCode = moduleTemplate.replace('{{compilerCode}}', compilerCode)

with open('npm/kaitai-struct-compiler.js','wt') as f: f.write(moduleCode)
for fn in ['LICENSE', 'README.md']:
    shutil.copy('../%s' % fn, 'npm/')
