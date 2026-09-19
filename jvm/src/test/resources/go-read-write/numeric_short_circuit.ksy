meta:
  id: numeric_short_circuit
params:
  - id: n
    type: u8
  - id: d
    type: s8
  - id: enabled
    type: bool
instances:
  and_div:
    value: 'd != 0 and n / d == 0'
  or_div:
    value: 'd == 0 or n / d == 0'
  and_mod:
    value: 'd != 0 and n % d == 0'
  or_mod:
    value: 'd == 0 or n % d == 0'
  guarded_div:
    value: 'enabled and n / (_io.pos + d) == 0'
  guarded_mod:
    value: 'enabled and n % (_io.pos + d) == 0'
  or_position_div:
    value: 'not enabled or n / (_io.pos + d) == 0'
  or_position_mod:
    value: 'not enabled or n % (_io.pos + d) == 0'
  nested:
    value: 'enabled and (d == 0 or (n / (_io.pos + d) == 0 and n % (_io.pos + d) == 0))'
  chained:
    value: 'enabled and d != 0 and n / (_io.pos + d) == 0 and n % (_io.pos + d) == 0'
  chained_or:
    value: 'not enabled or d == 0 or n / (_io.pos + d) == 0 or n % (_io.pos + d) == 0'
  both_operands:
    value: 'enabled and (n + _io.pos.as<u8>) / (_io.pos + d) == 0'
  pure_boolean:
    value: 'enabled and d != 0 or n == 0'
