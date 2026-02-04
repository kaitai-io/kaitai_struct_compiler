// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild



namespace Kaitai
{
    public partial class Simple : KaitaiStruct
    {
        public static Simple FromFile(string fileName)
        {
            return new Simple(new KaitaiStream(fileName));
        }

        public Simple(KaitaiStream p__io, KaitaiStruct p__parent = null, Simple p__root = null) : base(p__io)
        {
            m_parent = p__parent;
            m_root = p__root ?? this;
            _read();
        }
        private void _read()
        {
            _one = m_io.ReadU1();
        }
        public byte One { get { return _one; } }
        public Simple M_Root { get { return m_root; } }
        public KaitaiStruct M_Parent { get { return m_parent; } }
        private byte _one;
        private Simple m_root;
        private KaitaiStruct m_parent;
    }
}
