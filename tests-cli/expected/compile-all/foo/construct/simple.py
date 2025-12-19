from construct import *
from construct.lib import *

simple = Struct(
	'one' / Int8ub,
)

_schema = simple
