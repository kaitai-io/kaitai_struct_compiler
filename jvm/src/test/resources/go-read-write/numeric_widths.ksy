meta:
  id: numeric_widths
  endian: le
params:
  - id: narrow_min
    type: s1
seq:
  - id: small
    type: s1
  - id: medium
    type: s2
  - id: unsigned
    type: u4
  - id: wide
    type: u8
  - id: blocks
    type: block(_index)
    repeat: expr
    repeat-expr: 2
  - id: processed
    type: processed_byte
    size: 1
    process: xor((_index << 4) | _io.pos)
    repeat: expr
    repeat-expr: 2
  - id: switched
    type:
      switch-on: small
      cases:
        -2: u2le
        _: u2be
types:
  processed_byte:
    seq:
      - id: value
        type: u1
  block:
    params:
      - id: idx
        type: s4
    seq:
      - id: value
        type: u1
        valid: idx
instances:
  sum:
    value: small + medium
  negated_min:
    value: -narrow_min
  doubled_medium:
    value: medium * 200
  mixed_sum:
    value: unsigned + small
  mixed_less:
    value: small < unsigned
  choice_small:
    value: 'true ? small : medium'
  choice_medium:
    value: 'false ? small : medium'
  computed_bytes:
    value: '[small + 3, medium - 45].as<bytes>'
  wide_choice:
    value: '(false ? 1 : wide).as<u8>'
  wide_sum:
    value: '(wide + 5).as<u8>'
  wide_literal_choice:
    value: '(false ? wide : 0x8000_0000_0000_0000).as<u8>'
  wide_bits:
    value: '(wide | unsigned).as<u8>'
  shifted:
    value: '(unsigned << 4).as<u8>'
  modulo:
    value: small % medium
  negative_less_wide:
    value: small < wide
  wide_greater_negative:
    value: wide > small
  negative_not_equal_wide:
    value: small != wide
  wide_equal_negative:
    value: wide == small
  negative_greater_equal_wide:
    value: small >= wide
  wide_less_equal_negative:
    value: wide <= small
  negative_literal_less_wide:
    value: -2 < wide
  wide_literal_greater_negative:
    value: 0x8000_0000_0000_0000 > small
  modulo_position:
    value: small % _io.pos
