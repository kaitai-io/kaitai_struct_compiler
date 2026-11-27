meta:
  id: parent_navigation
instances:
  parent:
    value: 7
seq:
  - id: main
    type: foo
  - id: forced
    type: foo::zero
    parent: main
types:
  foo:
    seq:
      - id: kind
        type: u1
      - id: flag
        type: u1
      - id: buf
        type:
          switch-on: kind
          cases:
            0: zero
            1: one
    types:
      zero:
        seq:
          - id: branch
            type: common
      one:
        seq:
          - id: branch
            type: common
      common:
        instances:
          flag:
            value: _parent._parent.as<foo>.flag
          external_flag:
            value: _parent._parent._parent._parent.as<foo>.flag
