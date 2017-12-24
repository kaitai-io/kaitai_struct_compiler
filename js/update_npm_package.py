#!/usr/bin/env python2

import os
import errno
import shutil
import re
import subprocess
import datetime


def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise

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

with open('target/scala-2.12/kaitai-struct-compiler-fastopt.js','rt') as f: compilerCode = f.read()

moduleCode = moduleTemplate.replace('{{compilerCode}}', compilerCode)

mkdir_p('npm')

with open('npm/kaitai-struct-compiler.js','wt') as f: f.write(moduleCode)
for fn in ['../LICENSE', 'README.md']:
    shutil.copy(fn, 'npm/')

gitInfo = subprocess.check_output(['git log -1 --format=%H,%ct'], shell=True).strip().split(',')
commitId = gitInfo[0]
commitTs = int(gitInfo[1])
commitDate = datetime.datetime.fromtimestamp(commitTs).strftime('%Y%m%d.%H%M%S')

with open('package.json','rb') as f: packageJson = f.read()
packageJson = re.sub(r'("version": "\d+\.\d+\.\d+-SNAPSHOT.)[^"]*"', r'\g<1>%s"' % commitDate, packageJson)
with open('npm/package.json','wb') as f: f.write(packageJson)
