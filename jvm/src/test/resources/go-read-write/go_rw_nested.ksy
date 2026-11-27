meta:
  id: go_rw_nested
seq:
  - id: child
    type: child_type
    size: 2
types:
  child_type:
    seq:
      - id: value
        type: u2le
