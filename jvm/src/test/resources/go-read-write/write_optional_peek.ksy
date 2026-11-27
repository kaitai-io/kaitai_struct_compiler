meta:
  id: write_optional_peek
seq:
  - id: child
    type: body
    terminator: 0
    eos-error: false
    consume: false
types:
  body:
    seq:
      - id: data
        size-eos: true
