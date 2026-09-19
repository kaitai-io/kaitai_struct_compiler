meta:
  id: write_instance_nested
seq:
  - id: prefix
    type: u1
instances:
  child:
    pos: 4
    size: 4
    process: xor(17)
    type: nested
  suffix:
    pos: 10
    type: u1
types:
  nested:
    seq:
      - id: prefix
        type: u1
      - id: offset
        type: u1
    instances:
      payload:
        pos: offset
        type: u2le
