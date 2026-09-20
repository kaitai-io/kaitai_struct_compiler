meta:
  id: write_repeat_bits_le
  bit-endian: le
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
