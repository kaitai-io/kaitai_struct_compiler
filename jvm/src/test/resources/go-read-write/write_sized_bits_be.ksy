meta:
  id: write_sized_bits_be
  bit-endian: be
seq:
  - id: child
    type: part
    size: 1
  - id: tail
    type: b5
types:
  part:
    seq:
      - id: value
        type: b3
