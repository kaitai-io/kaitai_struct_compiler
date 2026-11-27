meta:
  id: write_switch_child
seq:
  - id: tag
    type: u1
  - id: child
    type:
      switch-on: tag
      cases:
        1: body
    size: 3
    process: test_codec(7)
types:
  body:
    seq:
      - id: data
        size-eos: true
