meta:
  id: write_instance_nested_repeat
seq:
  - id: prefix
    type: u1
instances:
  children:
    pos: 2
    size: 2
    process: xor(17)
    type: nested
    repeat: expr
    repeat-expr: 2
types:
  nested:
    seq:
      - id: prefix
        type: u1
    instances:
      tail:
        pos: 1
        type: u1
