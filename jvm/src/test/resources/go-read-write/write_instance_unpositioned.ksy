meta:
  id: write_instance_unpositioned
seq:
  - id: prefix
    type: u1
  - id: payload
    size: count
  - id: suffix
    type: u1
instances:
  count:
    type: u1
