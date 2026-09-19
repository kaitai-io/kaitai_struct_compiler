package contract_test

import (
	"bytes"
	c "contract"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"testing"
)

func TestConstructNestedGraphWithoutRead(t *testing.T) {
	root := c.NewWriteNested()
	root.SetRoot_(root)
	direct := c.NewWriteNested_Leaf()
	direct.SetRoot_(root)
	direct.SetParent_(root)
	direct.SetValue(17)
	middle := c.NewWriteNested_Middle()
	middle.SetRoot_(root)
	middle.SetParent_(root)
	middle.SetValue(51)
	leaf := c.NewWriteNested_Leaf()
	leaf.SetRoot_(root)
	leaf.SetParent_(middle)
	leaf.SetValue(34)
	if err := direct.Check(); err != nil {
		t.Fatal(err)
	}
	if err := leaf.Check(); err != nil {
		t.Fatal(err)
	}
	middle.SetChild(leaf)
	if err := middle.Check(); err != nil {
		t.Fatal(err)
	}
	root.SetDirect(direct)
	root.SetChild(middle)
	root.SetSize(2)
	root.SetTail(68)
	if err := root.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := root.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{17, 2, 34, 51, 68}) {
		t.Fatalf("got %x", out.Bytes())
	}
	for _, invalidate := range []func(){func() { leaf.SetRoot_(root) }, func() { leaf.SetParent_(middle) }} {
		invalidate()
		var unchecked kaitai.ConsistencyNotCheckedError
		if err := leaf.Write(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil))); !errors.As(err, &unchecked) {
			t.Fatalf("metadata setter retained checked state: %v", err)
		}
		if err := leaf.Check(); err != nil {
			t.Fatal(err)
		}
	}
}
