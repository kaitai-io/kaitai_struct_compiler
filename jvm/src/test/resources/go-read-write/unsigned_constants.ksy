meta:
  id: unsigned_constants
  endian: be
seq:
  - id: value
    type: u8
    enum: values
  - id: child
    type: nested
enums:
  values:
    0: zero
    9223372036854775807: signed_max
    9223372036854775808: signed_over_max
    18446744073709551615: unsigned_max
instances:
  decimal:
    value: 18446744073709551615
  hexadecimal:
    value: 0xffffffffffffffff
  signed_over_max:
    value: 9223372036854775808
  signed_max:
    value: 9223372036854775807
  alias:
    value: decimal
  less:
    value: decimal > -1
  difference:
    value: decimal - signed_over_max
  enum_integer:
    value: value.to_i
  child_max:
    value: child.maximum
  choice:
    value: 'value == values::zero ? decimal : child.maximum'
types:
  nested:
    enums:
      signed:
        -9223372036854775808: minimum
        9223372036854775807: maximum
    instances:
      maximum:
        value: 18446744073709551615
      parent_max:
        value: _root.decimal
