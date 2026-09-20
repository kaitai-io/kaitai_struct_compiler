meta:
  id: write_processed_child
params:
  - id: encoded_size
    type: u1
seq:
  - id: child
    size: encoded_size
    type: body
    process: test_codec(7)
  - id: tail
    type: u1
types:
  body:
    seq:
      - id: data
        size-eos: true
