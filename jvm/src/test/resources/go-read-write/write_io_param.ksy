meta:
  id: write_io_param
seq:
  - id: child
    type: body(_io)
    size: 2
  - id: tail
    type: u1
types:
  body:
    params:
      - id: source
        type: io
    seq:
      - id: data
        size: source.pos
