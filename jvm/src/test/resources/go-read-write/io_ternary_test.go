package contract

import (
	"bytes"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"testing"
)

// Deliberately satisfies only the existing runtime contract.
type readOnlyIo struct {
	stream *kaitai.Stream
	calls  int
}

func (s *readOnlyIo) IO_() *kaitai.Stream { s.calls++; return s.stream }

func TestReadIoSelection(t *testing.T) {
	for _, tc := range []struct {
		flag  byte
		size  int64
		plus  int64
		input []byte
	}{
		{0x40, 4, 7, []byte{0x40, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3}},
		{0, 8, 11, []byte{0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3}},
	} {
		p := NewIoTernary()
		if err := p.Read(kaitai.NewStream(bytes.NewReader(tc.input)), nil, p); err != nil {
			t.Fatal(err)
		}
		var selected kaitai.Struct = p.Obj2
		if tc.flag == 0x40 {
			selected = p.Obj1
		}
		got, err := p.Selected()
		if err != nil || got != selected {
			t.Fatalf("selected %T %v", got, err)
		}
		if tc.flag == 0x40 {
			if got.(*IoTernary_One) != p.Obj1 {
				t.Fatal("lost concrete one identity")
			}
		} else {
			if got.(*IoTernary_Two) != p.Obj2 {
				t.Fatal("lost concrete two identity")
			}
		}
		same, err := p.SameClass()
		if err != nil || same != p.Obj1 {
			t.Fatalf("same class %v %v", same, err)
		}
		lazy, err := p.LazySelected()
		if err != nil || lazy != selected {
			t.Fatalf("lazy selection %T %v", lazy, err)
		}
		for name, query := range map[string]func() (*kaitai.Stream, error){"direct": p.Stream, "cached object": p.ObjectStream, "nested": p.NestedStream, "chained": p.ChainedStream, "lazy": p.LazyStream, "lazy false": p.LazyFalse, "lazy nested": p.LazyNested} {
			stream, err := query()
			if err != nil || stream != selected.IO_() {
				t.Fatalf("%s identity %p %v", name, stream, err)
			}
			again, err := query()
			if err != nil || again != stream {
				t.Fatalf("%s cache %p %v", name, again, err)
			}
		}
		for name, query := range map[string]func() (int, error){"direct": p.DirectSize, "cached": p.CachedSize} {
			size, err := query()
			if err != nil || int64(size) != tc.size {
				t.Fatalf("%s size %v %v", name, size, err)
			}
		}
		plus, err := p.PlusThree()
		if err != nil || int64(plus) != tc.plus {
			t.Fatalf("plus %v %v", plus, err)
		}
		if int64(len(p.Payload)) != tc.size {
			t.Fatal("sequence IO size")
		}
		if p._f_bad {
			t.Fatal("unselected lazy branch evaluated")
		}
		if _, err := p.Bad(); err == nil {
			t.Fatal("erroring branch must fail when selected")
		}
	}
}

func TestReadIoCapability(t *testing.T) {
	input := kaitai.NewStream(bytes.NewReader([]byte{1, 2, 3, 4, 5}))
	parent := &readOnlyIo{stream: input}
	probe := NewIoTernary_Probe(parent)
	stream, err := probe.Stream()
	if err != nil || stream != input || parent.calls != 1 {
		t.Fatalf("stream %p %v calls %d", stream, err, parent.calls)
	}
	if _, err := probe.Stream(); err != nil || parent.calls != 1 {
		t.Fatal("cached IO re-evaluated")
	}
	size, err := probe.Length()
	if err != nil || size != 5 || parent.calls != 2 {
		t.Fatalf("size %v %v calls %d", size, err, parent.calls)
	}
	for _, invalid := range []kaitai.Struct{nil, (*readOnlyIo)(nil), NewIoTernary_One(), &readOnlyIo{}} {
		probe := NewIoTernary_Probe(invalid)
		if _, err := probe.Stream(); err == nil {
			t.Fatalf("nil IO accepted from %T", invalid)
		}
	}
}
