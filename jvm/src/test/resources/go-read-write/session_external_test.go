package contract_test

import (
	"bytes"
	c "contract"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"testing"
)

func TestSessionChildFollowedByCallerBits(t *testing.T) {
	p := c.NewSessionBits()
	p.SetRoot_(p)
	child := c.NewSessionBits_Body()
	child.SetParent_(p)
	child.SetRoot_(p)
	child.SetValue(0x42)
	p.SetChild(child)
	p.SetTail(5)
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	for _, shared := range []bool{false, true} {
		buf := kaitai.NewSeekableBuffer(nil)
		w := kaitai.NewWriter(buf)
		if shared {
			context := kaitai.NewWriteContext()
			if err := p.WriteWithContext_(w, context); err != nil {
				t.Fatal(err)
			}
			if err := context.Finish(); err != nil {
				t.Fatal(err)
			}
			context.Close()
		} else if err := p.WriteNoAlign_(w); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(buf.Bytes(), []byte{0x42}) {
			t.Fatalf("premature bit alignment %x", buf.Bytes())
		}
		if pos, err := w.Pos(); err != nil || pos != 2 {
			t.Fatalf("logical position %d %v", pos, err)
		}
		if err := w.WriteBitsIntBe(5, 19); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(buf.Bytes(), []byte{0x42, 0xb3}) {
			t.Fatalf("shared bytes %x", buf.Bytes())
		}
	}
}
