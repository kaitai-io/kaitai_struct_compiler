meta:
  id: write_instance_child_reference
seq:
  - id: payload
    size: child.count
    if: child.count > 0
  - id: child
    type: descriptor_type
  - id: suffix
    type: u1
types:
  descriptor_type:
    seq:
      - id: value
        type: u1
    instances:
      count:
        pos: 5
        type: u1
