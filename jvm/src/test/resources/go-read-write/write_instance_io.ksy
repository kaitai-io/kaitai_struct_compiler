meta:
  id: write_instance_io
params:
  - id: input
    type: io
seq:
  - id: prefix
    type: u1
instances:
  data:
    io: input
    pos: 2
    type: u1
