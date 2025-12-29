// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['exports', 'kaitai-struct/KaitaiStream'], factory);
  } else if (typeof exports === 'object' && exports !== null && typeof exports.nodeType !== 'number') {
    factory(exports, require('kaitai-struct/KaitaiStream'));
  } else {
    factory(root.Simple || (root.Simple = {}), root.KaitaiStream);
  }
})(typeof self !== 'undefined' ? self : this, function (Simple_, KaitaiStream) {
var Simple = (function() {
  function Simple(_io, _parent, _root) {
    this._io = _io;
    this._parent = _parent;
    this._root = _root || this;

    this._read();
  }
  Simple.prototype._read = function() {
    this.one = this._io.readU1();
  }

  return Simple;
})();
Simple_.Simple = Simple;
});
