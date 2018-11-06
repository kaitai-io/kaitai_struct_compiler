#!/bin/sh -ef

. ./lib_bintray.sh

# Config
BINTRAY_USER=greycat
BINTRAY_ACCOUNT=kaitai-io
BINTRAY_REPO=universal_unstable
BINTRAY_PACKAGE=kaitai-struct-compiler
BINTRAY_VERSION="$KAITAI_STRUCT_VERSION"
# BINTRAY_API_KEY comes from encrypted variables from web UI

#BINTRAY_CURL_ARGS=-v

bintray_create_version
bintray_upload_generic "jvm/target/universal/kaitai-struct-compiler-${KAITAI_STRUCT_VERSION}.zip"
bintray_publish_version
