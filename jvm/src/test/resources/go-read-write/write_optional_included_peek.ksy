meta:
  id: write_optional_included_peek
seq:
  - id: child
    type: body
    terminator: 0
    eos-error: false
    include: true
    consume: false
types:
  body:
    seq:
      - id: data
        size-eos: true
