meta:
  id: write_repeat_bits_be
  bit-endian: be
seq:
  - id: children
    type: part
    repeat: expr
    repeat-expr: 2
  - id: tail
    type: b2
types:
  part:
    seq:
      - id: value
        type: b3
