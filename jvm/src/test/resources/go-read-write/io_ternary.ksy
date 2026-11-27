meta:
  id: io_ternary
seq:
  - id: flag
    type: u1
  - id: obj1
    size: 4
    type: one
  - id: obj2
    size: 8
    type: two
  - id: payload
    size: '(flag == 0x40 ? obj1 : obj2)._io.size'
types:
  one:
    seq:
      - id: value
        type: u1
  two:
    seq:
      - id: value
        type: u1
  probe:
    params:
      - id: owner
        type: struct
    instances:
      stream:
        value: owner._io
      length:
        value: owner._io.size
      at_end:
        value: owner._io.eof
instances:
  selected:
    value: 'flag == 0x40 ? obj1 : obj2'
  stream:
    value: '(flag == 0x40 ? obj1 : obj2)._io'
  direct_size:
    value: '(flag == 0x40 ? obj1 : obj2)._io.size'
  cached_size:
    value: stream.size
  plus_three:
    value: '(flag == 0x40 ? obj1 : obj2)._io.size + 3'
  object_stream:
    value: selected._io
  nested_stream:
    value: '(flag == 0x40 ? (true ? obj1 : obj2) : (false ? obj1 : obj2))._io'
  chained_stream:
    value: 'flag == 0x40 ? obj1._io : obj2._io'
  same_class:
    value: 'true ? obj1 : bad'
  lazy_selected:
    value: 'true ? selected : bad'
  lazy_stream:
    value: 'true ? stream : bad._io'
  lazy_false:
    value: 'false ? bad._io : selected._io'
  lazy_nested:
    value: '(false ? bad : (true ? selected : bad))._io'
  bad:
    value: '"invalid".to_i > 0 ? obj1 : obj1'
