#pragma once

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

class simple_t;

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <memory>

#if KAITAI_STRUCT_VERSION < 11000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.11 or later is required"
#endif

class simple_t : public kaitai::kstruct {

public:

    simple_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = nullptr, simple_t* p__root = nullptr);

private:
    void _read();
    void _clean_up();

public:
    ~simple_t();
    uint8_t one() const { return m_one; }
    simple_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }

private:
    uint8_t m_one;
    simple_t* m__root;
    kaitai::kstruct* m__parent;
};
