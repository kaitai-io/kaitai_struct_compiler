meta:
  id: write_nested
seq:
  - id: direct
    type: leaf
  - id: size
    type: u1
  - id: child
    type: middle
    size: size
  - id: tail
    type: u1
types:
  leaf:
    seq:
      - id: value
        type: u1
  middle:
    seq:
      - id: child
        type: leaf
        size: 1
      - id: value
        type: u1
