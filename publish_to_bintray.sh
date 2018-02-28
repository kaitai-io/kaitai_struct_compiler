#!/bin/sh -ef

. ./lib_bintray.sh

# Config
BINTRAY_USER=greycat
BINTRAY_ACCOUNT=kaitai-io
BINTRAY_REPO=debian_unstable
BINTRAY_PACKAGE=kaitai-struct-compiler
BINTRAY_VERSION="$KAITAI_STRUCT_VERSION"
# BINTRAY_API_KEY comes from encrypted variables from web UI

BINTRAY_DEB_DISTRIBUTION=jessie
BINTRAY_DEB_ARCH=all
BINTRAY_DEB_COMPONENT=main

#BINTRAY_CURL_ARGS=-v

bintray_create_version
bintray_upload_deb "jvm/target/kaitai-struct-compiler_${KAITAI_STRUCT_VERSION}_all.deb"
bintray_publish_version
