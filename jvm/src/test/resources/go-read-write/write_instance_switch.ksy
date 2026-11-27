meta:
  id: write_instance_switch
  endian:
    switch-on: kind
    cases:
      1: le
      _: be
params:
  - id: kind
    type: u1
seq:
  - id: prefix
    type: u1
instances:
  data:
    pos: 2
    type:
      switch-on: kind
      cases:
        1: u2
        _: u4
  data_array:
    pos: 8
    type:
      switch-on: kind
      cases:
        1: u2
        _: u4
    repeat: expr
    repeat-expr: 2
