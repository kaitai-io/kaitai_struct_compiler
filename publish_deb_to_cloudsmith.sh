#!/bin/sh -ef

pip install --upgrade cloudsmith-cli

# CLOUDSMITH_API_KEY will come from secure environment variables
cloudsmith push deb \
	kaitai/debian-unstable/any-distro/any-version \
	"jvm/target/kaitai-struct-compiler_${KAITAI_STRUCT_VERSION}_all.deb"
