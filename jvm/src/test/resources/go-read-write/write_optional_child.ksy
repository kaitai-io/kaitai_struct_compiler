meta:
  id: write_optional_child
seq:
  - id: child
    type: body
    terminator: 0
    eos-error: false
types:
  body:
    seq:
      - id: data
        size-eos: true
