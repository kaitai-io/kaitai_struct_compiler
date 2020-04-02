#!/bin/sh

# js/npm is a directory where we deploy contents of NPM package that
# will be eventually published. When calling this script, this
# directory should already exist and be populated.

cd js/npm
echo "//registry.npmjs.org/:_authToken=$NPM_API_KEY" >$HOME/.npmrc
npm publish --tag next || exit 1
rm ~/.npmrc
