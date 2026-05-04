import kaitai_struct_nim_runtime
import options

type
  Simple* = ref object of KaitaiStruct
    `one`*: uint8
    `parent`*: KaitaiStruct

proc read*(_: typedesc[Simple], io: KaitaiStream, root: KaitaiStruct, parent: KaitaiStruct): Simple


proc read*(_: typedesc[Simple], io: KaitaiStream, root: KaitaiStruct, parent: KaitaiStruct): Simple =
  template this: untyped = result
  this = new(Simple)
  let root = if root == nil: cast[Simple](this) else: cast[Simple](root)
  this.io = io
  this.root = root
  this.parent = parent

  let oneExpr = this.io.readU1()
  this.one = oneExpr

proc fromFile*(_: typedesc[Simple], filename: string): Simple =
  Simple.read(newKaitaiFileStream(filename), nil, nil)

