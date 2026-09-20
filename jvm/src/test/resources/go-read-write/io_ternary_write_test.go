package contract

import (
	"bytes"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"testing"
)

type outputIo struct {
	writer *kaitai.Writer
	calls  int
}

func (s *outputIo) IO_() *kaitai.Stream     { panic("writer query reached read IO") }
func (s *outputIo) Writer_() *kaitai.Writer { s.calls++; return s.writer }

func TestFreshWriteIoSelection(t *testing.T) {
	for _, tc := range []struct {
		flag byte
		want []byte
		size int
		plus int
	}{
		{0x40, []byte{0x40, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3}, 4, 7},
		{0, []byte{0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3}, 8, 11},
	} {
		p := NewIoTernary()
		p.SetRoot_(p)
		p.SetFlag(tc.flag)
		one := NewIoTernary_One()
		one.SetRoot_(p)
		one.SetParent_(p)
		one.SetValue(1)
		two := NewIoTernary_Two()
		two.SetRoot_(p)
		two.SetParent_(p)
		two.SetValue(2)
		p.SetObj1(one)
		p.SetObj2(two)
		p.SetPayload(bytes.Repeat([]byte{3}, tc.size))
		for _, o := range []interface{ Check() error }{one, two, p} {
			if err := o.Check(); err != nil {
				t.Fatal(err)
			}
		}
		output := kaitai.NewFixedSeekableBuffer(len(tc.want))
		if err := p.Write(kaitai.NewWriter(output)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(output.Bytes(), tc.want) {
			t.Fatalf("write %x want %x", output.Bytes(), tc.want)
		}
		if p.IO_() != nil || one.IO_() != nil || two.IO_() != nil {
			t.Fatal("fresh object acquired read IO")
		}
		if one._activeWriter != nil || two._activeWriter != nil {
			t.Fatal("child write scopes still active")
		}
		selected := two.Writer_()
		var obj kaitai.Struct = two
		if tc.flag == 0x40 {
			selected = one.Writer_()
			obj = one
		}
		actual, err := p._writeGetSelected()
		if err != nil || actual != obj {
			t.Fatalf("writer selected %T %v", actual, err)
		}
		same, err := p._writeGetSameClass()
		if err != nil || same != one {
			t.Fatal("writer same class")
		}
		lazy, err := p._writeGetLazySelected()
		if err != nil || lazy != obj {
			t.Fatal("writer lazy selection")
		}
		for name, query := range map[string]func() (*kaitai.Writer, error){"direct": p._writeGetStream, "cached object": p._writeGetObjectStream, "nested": p._writeGetNestedStream, "chained": p._writeGetChainedStream, "lazy": p._writeGetLazyStream, "lazy false": p._writeGetLazyFalse, "lazy nested": p._writeGetLazyNested} {
			got, err := query()
			if err != nil || got != selected {
				t.Fatalf("writer %s %p %v", name, got, err)
			}
		}
		for name, query := range map[string]func() (int, error){"direct": p._writeGetDirectSize, "cached": p._writeGetCachedSize} {
			got, err := query()
			if err != nil || got != tc.size {
				t.Fatalf("writer %s %v %v", name, got, err)
			}
		}
		plus, err := p._writeGetPlusThree()
		if err != nil || plus != tc.plus {
			t.Fatalf("writer plus %v %v", plus, err)
		}
		if p._f_bad {
			t.Fatal("unselected writer branch evaluated")
		}
		if _, err := p._writeGetBad(); err == nil {
			t.Fatal("erroring writer branch must fail when selected")
		}
		again := NewIoTernary()
		if err := again.Read(kaitai.NewStream(bytes.NewReader(output.Bytes())), nil, again); err != nil {
			t.Fatal(err)
		}
		input, err := again.Stream()
		if err != nil {
			t.Fatal(err)
		}
		if err := again.Check(); err != nil {
			t.Fatal(err)
		}
		if err := again.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(len(tc.want)))); err != nil {
			t.Fatal(err)
		}
		cached, err := again.Stream()
		if err != nil || cached != input {
			t.Fatal("write changed read stream cache")
		}
		written, err := again._writeGetStream()
		if err != nil || written == nil {
			t.Fatal("writer stream cache unavailable")
		}
	}
}

func TestWriterIoCapability(t *testing.T) {
	writer := kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(5))
	parent := &outputIo{writer: writer}
	probe := NewIoTernary_Probe(parent)
	got, err := probe._writeGetStream()
	if err != nil || got != writer || parent.calls != 1 {
		t.Fatalf("writer %p %v calls %d", got, err, parent.calls)
	}
	size, err := probe._writeGetLength()
	if err != nil || size != 5 || parent.calls != 2 {
		t.Fatalf("size %v %v calls %d", size, err, parent.calls)
	}
	atEnd, err := probe._writeGetAtEnd()
	if err != nil || atEnd {
		t.Fatalf("eof %v %v", atEnd, err)
	}
	for _, invalid := range []kaitai.Struct{nil, (*outputIo)(nil), (*IoTernary_One)(nil), NewIoTernary_One(), &readOnlyIo{}, &outputIo{}} {
		probe := NewIoTernary_Probe(invalid)
		for attempt := 0; attempt < 2; attempt++ {
			if _, err := probe._writeGetStream(); err == nil {
				t.Fatalf("missing writer accepted from %T", invalid)
			}
		}
	}
	nonseek := &outputIo{writer: kaitai.NewWriter(&bytes.Buffer{})}
	probe = NewIoTernary_Probe(nonseek)
	if _, err := probe._writeGetLength(); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
		t.Fatalf("nonseekable size: %v", err)
	}
	if _, err := probe._writeGetAtEnd(); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
		t.Fatalf("nonseekable eof: %v", err)
	}
	var empty *IoTernary_One
	if empty.Writer_() != nil {
		t.Fatal("nil generated Writer_ should be safe")
	}
}
