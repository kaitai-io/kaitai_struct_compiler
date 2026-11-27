meta:
  id: unsigned_array_compositions
  endian: be
seq:
  - id: input
    type: u8
instances:
  calculated:
    value: input + 0
  values:
    value: '[calculated]'
  choice:
    value: 'input > 0 ? values : [256]'
  choice_first:
    value: choice[0]
  small_values:
    value: '[256]'
  named_choice:
    value: 'input > 0 ? values : small_values'
  scalar_array:
    value: '[small_values[0], calculated]'
  matrix:
    value: '[values]'
  mixed_matrix:
    value: '[values, small_values]'
  matrix_value:
    value: matrix[0][0]
  bounded:
    value: input.as<u4> + 0
  bounded_values:
    value: '[bounded.as<u8>]'
  signed_from_values:
    value: '[bounded_values[0], -1]'
  signed_values:
    value: '[bounded.as<u8>, -1]'
  signed_positive:
    value: signed_values[0]
  signed_negative:
    value: signed_values[1]
  signed_alias:
    value: signed_positive
