#!/bin/sh -ef

pip install --upgrade cloudsmith-cli

cloudsmith push deb \
	kaitai/debian-unstable/any-distro/any-version \
	"jvm/target/kaitai-struct-compiler_${KAITAI_STRUCT_VERSION}_all.deb"
