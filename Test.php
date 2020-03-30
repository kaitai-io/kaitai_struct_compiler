<?php
// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

namespace {
    class Test extends \Kaitai\Struct\Struct {
        public function __construct(\Kaitai\Struct\Stream $_io, \Kaitai\Struct\Struct $_parent = null, \Test $_root = null) {
            parent::__construct($_io, $_parent, $_root);
            $this->_read();
        }

        private function _read() {
            $this->_m_name = \Kaitai\Struct\Stream::bytesToStr($this->_io->readBytes(2), "UTF-8");
            if (!(preg_match("/\d\d/", $this->name()))) {
                throw new \Kaitai\Struct\Error\ValidationRegexMatchError("\\d\\d", $this->name(), $this->_io(), "/seq/0");
            }
        }
        protected $_m_name;
        public function name() { return $this->_m_name; }
    }
}
