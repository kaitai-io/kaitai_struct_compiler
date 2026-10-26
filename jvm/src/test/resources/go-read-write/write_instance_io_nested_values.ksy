meta:
  id: write_instance_io_nested_values
seq:
  - id: prefix
    type: u1
instances:
  matrix:
    value: '[[_io], cast_outputs]'
  cube:
    value: '[[[_io]]]'
  cast_outputs:
    value: direct_cast_outputs.as<io[]>
  direct_cast_outputs:
    value: '[_io].as<io[]>'
  cast_output:
    value: _io.as<io>
  from_matrix:
    io: matrix[0][0]
    pos: 2
    type: u1
  from_cube:
    io: cube[0][0][0]
    pos: 4
    type: u1
  from_cast_outputs:
    io: cast_outputs[0]
    pos: 6
    type: u1
  from_cast_output:
    io: cast_output
    pos: 8
    type: u1
