meta:
  id: write_nested_bits_le
  bit-endian: le
seq:
  - id: child
    type: part
  - id: tail
    type: b5
types:
  part:
    seq:
      - id: value
        type: b3
