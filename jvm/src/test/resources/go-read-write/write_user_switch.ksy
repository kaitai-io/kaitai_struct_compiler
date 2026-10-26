meta:
  id: write_user_switch
seq:
  - id: tag
    type: u1
  - id: child
    type:
      switch-on: tag
      cases:
        1: one
        2: two
types:
  one:
    seq:
      - id: value
        type: u1
  two:
    seq:
      - id: value
        type: u2le
