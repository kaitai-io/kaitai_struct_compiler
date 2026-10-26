meta:
  id: write_nested_bits_be
  bit-endian: be
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
