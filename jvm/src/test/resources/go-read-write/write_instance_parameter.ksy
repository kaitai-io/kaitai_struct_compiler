meta:
  id: write_instance_parameter
params:
  - id: descriptor
    type: descriptor_type
seq:
  - id: payload
    size: descriptor.count
    if: descriptor.count > 0
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
