meta:
  id: write_position_child
seq:
  - id: prefix
    type: u1
  - id: child
    type: body
    size: _io.size - _io.pos
types:
  body:
    seq:
      - id: data
        size-eos: true
