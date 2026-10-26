meta:
  id: write_zlib_child
params:
  - id: encoded_size
    type: u1
seq:
  - id: child
    size: encoded_size
    type: body
    process: zlib
types:
  body:
    seq:
      - id: data
        size-eos: true
