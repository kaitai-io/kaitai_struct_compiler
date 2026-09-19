meta:
  id: write_optional_included
seq:
  - id: child
    type: body
    terminator: 0
    eos-error: false
    include: true
types:
  body:
    seq:
      - id: data
        size-eos: true
