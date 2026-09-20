meta:
  id: write_instance_io_conditional
params:
  - id: other
    type: io
seq:
  - id: flag
    type: u1
  - id: prefix
    type: u1
instances:
  scalar:
    value: 'flag == 1 ? _io : other'
  array:
    value: 'flag == 1 ? [_io] : [other]'
  matrix:
    value: 'flag == 1 ? [[_io]] : [[other]]'
  from_scalar:
    io: scalar
    pos: 4
    type: u1
  from_array:
    io: array[0]
    pos: 6
    type: u1
  from_matrix:
    io: matrix[0][0]
    pos: 8
    type: u1
