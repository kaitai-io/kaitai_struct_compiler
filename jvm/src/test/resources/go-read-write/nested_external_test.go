package contract_test

import (
	"bytes"
	c "contract"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"io"
	"testing"
)

type externalBitPart interface {
	SetValue(uint64)
	Check() error
	Write(*kaitai.Writer) error
	WriteNoAlign_(*kaitai.Writer) error
}

func TestExternalPackageUnalignedNestedWrite(t *testing.T) {
	for _, tc := range []struct {
		name             string
		makePart         func() externalBitPart
		tail             func(*kaitai.Writer) error
		combined, padded byte
	}{
		{"be", func() externalBitPart { return c.NewWriteNestedBitsBe_Part() }, func(w *kaitai.Writer) error { return w.WriteBitsIntBe(5, 19) }, 0xb3, 0xa0},
		{"le", func() externalBitPart { return c.NewWriteNestedBitsLe_Part() }, func(w *kaitai.Writer) error { return w.WriteBitsIntLe(5, 19) }, 0x9d, 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			child := tc.makePart()
			child.SetValue(5)
			for _, write := range []func(*kaitai.Writer) error{child.Write, child.WriteNoAlign_} {
				var unchecked kaitai.ConsistencyNotCheckedError
				if err := write(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil))); !errors.As(err, &unchecked) {
					t.Fatalf("unchecked: %v", err)
				}
			}
			if err := child.Check(); err != nil {
				t.Fatal(err)
			}
			out := kaitai.NewSeekableBuffer(nil)
			writer := kaitai.NewWriter(out)
			if err := child.WriteNoAlign_(writer); err != nil {
				t.Fatal(err)
			}
			if len(out.Bytes()) != 0 {
				t.Fatalf("nested helper aligned: %x", out.Bytes())
			}
			if err := tc.tail(writer); err != nil {
				t.Fatal(err)
			}
			if err := writer.AlignToByte(); err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(out.Bytes(), []byte{tc.combined}) {
				t.Fatalf("external nested bits: %x", out.Bytes())
			}
			out = kaitai.NewSeekableBuffer(nil)
			if err := child.Write(kaitai.NewWriter(out)); err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(out.Bytes(), []byte{tc.padded}) {
				t.Fatalf("public Write finalization: %x", out.Bytes())
			}
			if err := child.Write(kaitai.NewWriter(externalWriteFailure{})); !errors.Is(err, io.ErrClosedPipe) {
				t.Fatalf("finalization failure: %v", err)
			}
		})
	}
}

type externalWriteFailure struct{}

func (externalWriteFailure) Write([]byte) (int, error) { return 0, io.ErrClosedPipe }

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
