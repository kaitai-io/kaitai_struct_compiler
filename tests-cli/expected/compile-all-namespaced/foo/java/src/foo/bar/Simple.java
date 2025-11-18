// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

package foo.bar;

import io.kaitai.struct.ByteBufferKaitaiStream;
import io.kaitai.struct.KaitaiStruct;
import io.kaitai.struct.KaitaiStream;
import java.io.IOException;

public class Simple extends KaitaiStruct {
    public static Simple fromFile(String fileName) throws IOException {
        return new Simple(new ByteBufferKaitaiStream(fileName));
    }

    public Simple(KaitaiStream _io) {
        this(_io, null, null);
    }

    public Simple(KaitaiStream _io, KaitaiStruct _parent) {
        this(_io, _parent, null);
    }

    public Simple(KaitaiStream _io, KaitaiStruct _parent, Simple _root) {
        super(_io);
        this._parent = _parent;
        this._root = _root == null ? this : _root;
        _read();
    }
    private void _read() {
        this.one = this._io.readU1();
    }

    public void _fetchInstances() {
    }
    public int one() { return one; }
    public Simple _root() { return _root; }
    public KaitaiStruct _parent() { return _parent; }
    private int one;
    private Simple _root;
    private KaitaiStruct _parent;
}
