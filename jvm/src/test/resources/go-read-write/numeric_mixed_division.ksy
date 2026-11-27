meta:
  id: numeric_mixed_division
params:
  - id: signed_value
    type: s8
  - id: unsigned_value
    type: u8
instances:
  signed_div_unsigned:
    value: '(signed_value / unsigned_value).as<s8>'
  unsigned_div_signed:
    value: '(unsigned_value / signed_value).as<s8>'
  signed_mod_unsigned:
    value: '(signed_value % unsigned_value).as<u8>'
  unsigned_mod_signed:
    value: '(unsigned_value % signed_value).as<u8>'
  position_div_unsigned:
    value: '((_io.pos - 7) / unsigned_value).as<s8>'
  position_mod_unsigned:
    value: '((_io.pos - 7) % unsigned_value).as<u8>'
  unsigned_div_position:
    value: '(unsigned_value / (_io.pos - 7)).as<s8>'
  unsigned_mod_position:
    value: '(unsigned_value % (_io.pos + 3)).as<u8>'
  literal_div_unsigned:
    value: '(-6 / unsigned_value).as<s8>'
  literal_mod_unsigned:
    value: '(-2 % unsigned_value).as<u8>'
  minimum_div_unsigned:
    value: '(-9223372036854775808 / unsigned_value).as<s8>'
  minimum_mod_unsigned:
    value: '(-9223372036854775808 % unsigned_value).as<u8>'
