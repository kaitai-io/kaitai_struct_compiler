// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "simple.h"

simple_t::simple_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, simple_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root ? p__root : this;
    _read();
}

void simple_t::_read() {
    m_one = m__io->read_u1();
}

simple_t::~simple_t() {
    _clean_up();
}

void simple_t::_clean_up() {
}
