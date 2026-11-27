meta:
  id: write_processed
seq:
  - id: scalar
    size: 2
    process: xor(255)
  - id: key
    size: 3
    process: xor([1, 2])
  - id: left
    size: 1
    process: rol(3)
  - id: right
    size: 1
    process: ror(3)
  - id: padded
    size: 4
    pad-right: 255
    terminator: 0
    process: xor(128)
  - id: included
    size: 4
    pad-right: 255
    terminator: 0
    include: true
    process: xor(128)
  - id: compressed
    size-eos: true
    process: zlib
