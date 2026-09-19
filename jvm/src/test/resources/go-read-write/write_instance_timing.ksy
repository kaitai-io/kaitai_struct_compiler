meta:
  id: write_instance_timing
seq:
  - id: prefix
    type: u1
  - id: payload
    size: count
  - id: suffix
    type: u1
instances:
  marker:
    pos: _io.pos + 4
    type: u1
  count:
    value: marker
