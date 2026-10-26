meta:
  id: write_child_formats
seq:
  - id: padded
    type: body
    size: 4
    terminator: 0
    pad-right: 255
    process: xor(128)
  - id: included
    type: body
    size: 4
    terminator: 0
    include: true
    pad-right: 255
    process: xor(128)
  - id: terminated
    type: body
    terminator: 0
  - id: repeated
    type: body
    size: 1
    process: xor(128)
    repeat: expr
    repeat-expr: 2
  - id: peeked
    type: body
    terminator: 0
    consume: false
  - id: marker
    type: u1
  - id: included_term
    type: body
    terminator: 0
    include: true
types:
  body:
    seq:
      - id: data
        size-eos: true
