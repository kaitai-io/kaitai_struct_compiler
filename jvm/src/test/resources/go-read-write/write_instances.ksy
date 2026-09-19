meta:
  id: write_instances
seq:
  - id: prefix
    type: u1
instances:
  marker:
    pos: 3
    type: u2le
  array:
    pos: 6
    type: u1
    repeat: expr
    repeat-expr: 2
  lazy:
    pos: 100
    type: u1
  derived:
    value: prefix * 2
