meta:
  id: numeric_unary
params:
  - id: unsigned32
    type: u4
  - id: unsigned64
    type: u8
  - id: signed8
    type: s1
instances:
  literal_invert_equal:
    value: ~0x80000000 == unsigned32
  literal_invert_signed:
    value: '(~0x80000000).as<s8>'
  variable_invert_signed:
    value: '(~unsigned32).as<s8>'
  small_variable_invert:
    value: ~signed8
  small_literal_invert:
    value: ~127
  wide_variable_invert:
    value: '(~unsigned64).as<u8>'
  minimum_signed:
    value: '(-9223372036854775808).as<s8>'
  minimum_less_unsigned32:
    value: -9223372036854775808 < unsigned32
  unsigned32_greater_minimum:
    value: unsigned32 > -9223372036854775808
  minimum_less_unsigned64:
    value: -9223372036854775808 < unsigned64
  unsigned64_greater_minimum:
    value: unsigned64 > -9223372036854775808
  minimum_equals_unsigned64:
    value: -9223372036854775808 == unsigned64
  unsigned64_equals_minimum:
    value: unsigned64 == -9223372036854775808
