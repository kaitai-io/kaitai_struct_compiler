meta:
  id: write_instance_io_values
seq:
  - id: prefix
    type: u1
  - id: suffix
    type: u1
    if: outputs[0].pos == 1
instances:
  output:
    value: _io
  outputs:
    value: '[output]'
  unused_output:
    value: _io
  unused_outputs:
    value: '[_io]'
  scalar:
    io: output
    pos: 3
    type: u1
  array:
    io: outputs[0]
    pos: 5
    type: u1
