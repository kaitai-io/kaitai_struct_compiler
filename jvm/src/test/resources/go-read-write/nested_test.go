package contract

import (
	"bytes"
	"compress/zlib"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"io"
	"testing"
)

var codecFailure = errors.New("codec failure")

func TestUnsizedNestedBitsShareByte(t *testing.T) {
	be := NewWriteNestedBitsBe()
	inputBe := []byte{0xb3}
	if err := be.Read(kaitai.NewStream(bytes.NewReader(inputBe)), nil, be); err != nil {
		t.Fatal(err)
	}
	if be.GetChild().GetValue() != 5 || be.GetTail() != 19 {
		t.Fatal("BE read fixture")
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := be.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputBe) {
		t.Errorf("BE child padded: %x", out.Bytes())
	}
	le := NewWriteNestedBitsLe()
	inputLe := []byte{0x9d}
	if err := le.Read(kaitai.NewStream(bytes.NewReader(inputLe)), nil, le); err != nil {
		t.Fatal(err)
	}
	if le.GetChild().GetValue() != 5 || le.GetTail() != 19 {
		t.Fatal("LE read fixture")
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := le.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputLe) {
		t.Fatalf("LE child padded: %x", out.Bytes())
	}
}

func TestRepeatedUnsizedBitsShareByte(t *testing.T) {
	be := NewWriteRepeatBitsBe()
	inputBe := []byte{0xae}
	if err := be.Read(kaitai.NewStream(bytes.NewReader(inputBe)), nil, be); err != nil {
		t.Fatal(err)
	}
	if be.GetChildren()[0].GetValue() != 5 || be.GetChildren()[1].GetValue() != 3 || be.GetTail() != 2 {
		t.Fatal("BE repeat fixture")
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := be.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputBe) {
		t.Errorf("BE repeated padding: %x", out.Bytes())
	}
	le := NewWriteRepeatBitsLe()
	inputLe := []byte{0x9d}
	if err := le.Read(kaitai.NewStream(bytes.NewReader(inputLe)), nil, le); err != nil {
		t.Fatal(err)
	}
	if le.GetChildren()[0].GetValue() != 5 || le.GetChildren()[1].GetValue() != 3 || le.GetTail() != 2 {
		t.Fatal("LE repeat fixture")
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := le.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputLe) {
		t.Fatalf("LE repeated padding: %x", out.Bytes())
	}
}

func TestSizedNestedBitsFinalizeIndependentStreams(t *testing.T) {
	be := NewWriteSizedBitsBe()
	inputBe := []byte{0xa0, 0x98}
	if err := be.Read(kaitai.NewStream(bytes.NewReader(inputBe)), nil, be); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := be.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputBe) {
		t.Fatalf("BE sized child: %x", out.Bytes())
	}
	le := NewWriteSizedBitsLe()
	inputLe := []byte{5, 19}
	if err := le.Read(kaitai.NewStream(bytes.NewReader(inputLe)), nil, le); err != nil {
		t.Fatal(err)
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := le.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), inputLe) {
		t.Fatalf("LE sized child: %x", out.Bytes())
	}
}

func TestOutputStreamArrayParameters(t *testing.T) {
	v := NewParamsPassArrayIo()
	input := []byte{17, 42}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewFixedSeekableBuffer(2)
	writer := kaitai.NewWriter(out)
	if err := v.Write(writer); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), input) {
		t.Fatalf("IO array: %x", out.Bytes())
	}
	streams := v.GetOne().ArgStreamsWriter_()
	if len(streams) != 2 || streams[0] != v.GetFirst()._writer || streams[1] != writer {
		t.Fatal("IO array identities")
	}
	for _, streams := range [][]*kaitai.Writer{{nil, writer}, {writer, nil}} {
		v.GetOne().SetArgStreamsWriter_(streams)
		if err := v.GetOne().Check(); err != nil {
			t.Fatal(err)
		}
		if err := v.GetOne().Write(writer); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
			t.Fatalf("nil IO array element: %v", err)
		}
	}
}

func TestUserOnlySwitchFetchAndWrite(t *testing.T) {
	for _, input := range [][]byte{{1, 42}, {2, 0x34, 0x12}} {
		v := NewWriteUserSwitch()
		if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
			t.Fatal(err)
		}
		if err := v.FetchInstances(); err != nil {
			t.Fatal(err)
		}
		if err := v.Check(); err != nil {
			t.Fatal(err)
		}
		out := kaitai.NewSeekableBuffer(nil)
		if err := v.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), input) {
			t.Fatalf("user-only switch: %x", out.Bytes())
		}
	}
}

func TestOptionalIncludedChildRequiresParentEof(t *testing.T) {
	for _, input := range [][]byte{{1, 2}, {1, 2, 0}, {}} {
		for _, extra := range []int{0, 1} {
			for mode := 0; mode < 2; mode++ {
				var obj interface{ Write(*kaitai.Writer) error }
				if mode == 0 {
					v := NewWriteOptionalIncluded()
					if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
						t.Fatal(err)
					}
					obj = v
				} else {
					v := NewWriteOptionalIncludedPeek()
					if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
						t.Fatal(err)
					}
					obj = v
				}
				out := kaitai.NewFixedSeekableBuffer(len(input) + extra)
				err := obj.Write(kaitai.NewWriter(out))
				absent := len(input) == 0 || input[len(input)-1] != 0
				if absent && extra != 0 {
					var consistency kaitai.ConsistencyError
					if !errors.As(err, &consistency) {
						t.Fatalf("mode %d: accepted absent terminator before parent EOF; bytes %x, error %v", mode, out.Bytes(), err)
					}
				} else {
					if err != nil {
						t.Fatal(err)
					}
					if !bytes.Equal(out.Bytes()[:len(input)], input) {
						t.Fatalf("mode %d: %x", mode, out.Bytes())
					}
				}
			}
		}
	}
}

type TestCodec struct{ key byte }

func NewTestCodec(key byte) *TestCodec { return &TestCodec{key} }
func (c *TestCodec) Decode(b []byte) ([]byte, error) {
	if len(b) > 0 && b[0] == 255 {
		return nil, codecFailure
	}
	if len(b) > 0 && b[0] == 42 {
		return b[1:], nil
	}
	for i := range b {
		b[i] ^= c.key
	}
	return b, nil
}
func (c *TestCodec) Encode(b []byte) ([]byte, error) {
	if len(b) > 0 && b[0] == 255 {
		return nil, codecFailure
	}
	if len(b) > 0 && b[0] == 254 {
		return b[:0], nil
	}
	if len(b) > 0 && b[0] == 253 {
		return append([]byte{42}, b...), nil
	}
	for i := range b {
		b[i] ^= c.key
	}
	return b, nil
}

func TestNestedExactAndRoundTrip(t *testing.T) {
	v := NewWriteNested()
	input := []byte{17, 2, 34, 51, 68}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), input) {
		t.Fatalf("got %x want %x", out.Bytes(), input)
	}
	if !bytes.Equal(v._raw_Child, []byte{34, 51}) {
		t.Fatalf("raw %x", v._raw_Child)
	}
}

func TestProcessedExactAndRoundTrip(t *testing.T) {
	v := NewWriteProcessed()
	v.SetScalar([]byte{0, 255})
	v.SetKey([]byte{0, 0, 0})
	v.SetLeft([]byte{1})
	v.SetRight([]byte{1})
	v.SetPadded([]byte{129})
	v.SetIncluded([]byte{129, 128})
	v.SetCompressed([]byte("hello hello hello"))
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	want := []byte{255, 0, 1, 2, 1, 32, 8, 1, 0, 255, 255, 1, 0, 255, 255}
	if !bytes.Equal(out.Bytes()[:len(want)], want) {
		t.Fatalf("got %x", out.Bytes())
	}
	read := NewWriteProcessed()
	if err := read.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())), nil, read); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(read.GetCompressed(), v.GetCompressed()) || !bytes.Equal(read.GetPadded(), v.GetPadded()) || !bytes.Equal(read.GetIncluded(), v.GetIncluded()) {
		t.Fatal("round trip")
	}
	if !bytes.Equal(v.GetScalar(), []byte{0, 255}) {
		t.Fatal("source mutated")
	}
}

func customChild(t *testing.T, data []byte, size uint8) *WriteProcessedChild {
	t.Helper()
	v := NewWriteProcessedChild(size)
	child := NewWriteProcessedChild_Body()
	child._parent = v
	child._root = v
	child.SetData(data)
	v._root = v
	v.SetChild(child)
	v.Child_InnerSize = len(data)
	v.SetTail(99)
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	return v
}

func TestCustomCodecPreservesSources(t *testing.T) {
	v := NewWriteCustom()
	v.SetData([]byte{1, 2})
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{6, 5}) || !bytes.Equal(v.GetData(), []byte{1, 2}) {
		t.Fatal("custom encoding or ownership")
	}
	r := NewWriteCustom()
	if err := r.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())), nil, r); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(r.GetData(), v.GetData()) || !bytes.Equal(r._raw_Data, []byte{6, 5}) {
		t.Fatal("custom decoding or raw ownership")
	}
	c := customChild(t, []byte{1, 2}, 2)
	out = kaitai.NewSeekableBuffer(nil)
	if err := c.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{6, 5, 99}) || !bytes.Equal(c._raw_Child, []byte{1, 2}) {
		t.Fatalf("custom child %x raw %x", out.Bytes(), c._raw_Child)
	}
}

func TestChildFailures(t *testing.T) {
	for _, tc := range []struct {
		data        []byte
		size        uint8
		cause       error
		consistency bool
	}{
		{[]byte{255}, 1, codecFailure, false}, {[]byte{254}, 1, nil, true}, {[]byte{1, 2}, 3, nil, true},
	} {
		v := customChild(t, tc.data, tc.size)
		out := kaitai.NewSeekableBuffer(nil)
		err := v.Write(kaitai.NewWriter(out))
		if tc.consistency {
			var e kaitai.ConsistencyError
			if !errors.As(err, &e) {
				t.Fatalf("consistency: %v", err)
			}
		} else if !errors.Is(err, tc.cause) {
			t.Fatalf("cause %v", err)
		}
		if len(out.Bytes()) != 0 {
			t.Fatalf("failure wrote parent %x", out.Bytes())
		}
	}
	v := customChild(t, []byte{1, 2}, 2)
	v.Child_InnerSize = 1
	if err := v.Write(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil))); err == nil {
		t.Fatal("expected inner overflow")
	}
	v = customChild(t, []byte{1, 2}, 2)
	if err := v.Write(kaitai.NewWriter(&bytes.Buffer{})); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
		t.Fatalf("nonseeker %v", err)
	}
	for _, seek := range []bool{false, true} {
		p := &failingParent{SeekableBuffer: kaitai.NewSeekableBuffer(nil), seek: seek}
		if err := v.Write(kaitai.NewWriter(p)); !errors.Is(err, io.ErrClosedPipe) {
			t.Fatalf("parent error %v", err)
		}
	}
}

func TestDifferentInnerOuterSizes(t *testing.T) {
	v := customChild(t, []byte{253, 2}, 3)
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{42, 253, 2, 99}) {
		t.Fatalf("got %x", out.Bytes())
	}
	r := NewWriteProcessedChild(3)
	if err := r.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())), nil, r); err != nil {
		t.Fatal(err)
	}
	if r.Child_InnerSize != 2 || !bytes.Equal(r.GetChild().GetData(), []byte{253, 2}) {
		t.Fatal("decoded size/data")
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := r.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{42, 253, 2, 99}) {
		t.Fatal("read/write round trip")
	}
}

func TestZlibChildRoundTrip(t *testing.T) {
	logical := bytes.Repeat([]byte("hello"), 20)
	var encoded bytes.Buffer
	zw := zlib.NewWriter(&encoded)
	if _, err := zw.Write(logical); err != nil {
		t.Fatal(err)
	}
	if err := zw.Close(); err != nil {
		t.Fatal(err)
	}
	v := NewWriteZlibChild(uint8(encoded.Len()))
	if err := v.Read(kaitai.NewStream(bytes.NewReader(encoded.Bytes())), nil, v); err != nil {
		t.Fatal(err)
	}
	if v.Child_InnerSize != len(logical) {
		t.Fatal("logical size")
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	zr, err := zlib.NewReader(bytes.NewReader(out.Bytes()))
	if err != nil {
		t.Fatal(err)
	}
	defer zr.Close()
	decoded, err := io.ReadAll(zr)
	if err != nil || !bytes.Equal(decoded, logical) {
		t.Fatal("zlib round trip", err)
	}
}

func TestChildFormattingAndRepeatedRaw(t *testing.T) {
	input := []byte{1, 0, 255, 255, 1, 0, 255, 255, 7, 0, 129, 130, 9, 0, 10, 0}
	v := NewWriteChildFormats()
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	for n := 0; n < 2; n++ {
		out := kaitai.NewSeekableBuffer(nil)
		if err := v.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), input) {
			t.Fatalf("got %x want %x", out.Bytes(), input)
		}
		if len(v._raw_Repeated) != 2 || len(v._raw__raw_Repeated) != 2 {
			t.Fatal("raw arrays grew on repeated write")
		}
	}
}

func TestChildOutputStreamParameter(t *testing.T) {
	v := NewWriteIoParam()
	input := []byte{1, 2, 3}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewFixedSeekableBuffer(3)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), input) {
		t.Fatalf("IO parameter: %x", out.Bytes())
	}
	v.GetChild().SetSourceWriter_(nil)
	if err := v.GetChild().Check(); err != nil {
		t.Fatal(err)
	}
	if err := v.GetChild().Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
		t.Fatalf("missing writer: %v", err)
	}
	source := kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(3))
	if _, err := source.Seek(2, io.SeekStart); err != nil {
		t.Fatal(err)
	}
	v.GetChild().SetSourceWriter_(source)
	if err := v.GetChild().Check(); err != nil {
		t.Fatal(err)
	}
	if err := v.GetChild().Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err != nil {
		t.Fatal(err)
	}
}

func TestSwitchedChildPreservesLogicalSize(t *testing.T) {
	v := NewWriteSwitchChild()
	input := []byte{1, 42, 253, 2}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), input) {
		t.Fatalf("switched child: %x", out.Bytes())
	}
}

func TestNestedOverflowAndFailurePropagation(t *testing.T) {
	v := NewWriteNested()
	if err := v.Read(kaitai.NewStream(bytes.NewReader([]byte{17, 2, 34, 51, 68})), nil, v); err != nil {
		t.Fatal(err)
	}
	v.SetSize(1)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); !errors.Is(err, kaitai.ErrBufferCapacity) {
		t.Fatalf("overflow: %v", err)
	}
	if !bytes.Equal(out.Bytes(), []byte{17, 1}) {
		t.Fatalf("overflow wrote failed child or suffix: %x", out.Bytes())
	}
}

func TestChildSizeUsesReservationPosition(t *testing.T) {
	v := NewWritePositionChild()
	input := []byte{1, 2, 3, 4}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewFixedSeekableBuffer(4)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), input) {
		t.Fatalf("position-dependent child: %x", out.Bytes())
	}
}

func TestOptionalTerminatedChild(t *testing.T) {
	for _, input := range [][]byte{{1, 2}, {1, 2, 0}, {}} {
		v := NewWriteOptionalChild()
		if err := v.Read(kaitai.NewStream(bytes.NewReader(input)), nil, v); err != nil {
			t.Fatal(err)
		}
		out := kaitai.NewFixedSeekableBuffer(len(input))
		if err := v.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), input) {
			t.Fatalf("got %x want %x", out.Bytes(), input)
		}
	}
}

func TestOptionalTerminatorIncludeConsume(t *testing.T) {
	for _, input := range [][]byte{{1, 2}, {1, 2, 0}, {}} {
		for mode := 0; mode < 3; mode++ {
			s := kaitai.NewStream(bytes.NewReader(input))
			var obj interface{ Write(*kaitai.Writer) error }
			var err error
			switch mode {
			case 0:
				v := NewWriteOptionalIncluded()
				err = v.Read(s, nil, v)
				obj = v
			case 1:
				v := NewWriteOptionalPeek()
				err = v.Read(s, nil, v)
				obj = v
			case 2:
				v := NewWriteOptionalIncludedPeek()
				err = v.Read(s, nil, v)
				obj = v
			}
			if err != nil {
				t.Fatal(err)
			}
			readPos, err := s.Pos()
			if err != nil {
				t.Fatal(err)
			}
			out := kaitai.NewFixedSeekableBuffer(len(input))
			w := kaitai.NewWriter(out)
			if err := obj.Write(w); err != nil {
				t.Fatal(err)
			}
			writePos, err := w.Pos()
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(out.Bytes(), input) || readPos != writePos {
				t.Fatalf("mode %d: got %x want %x; positions %d/%d", mode, out.Bytes(), input, readPos, writePos)
			}
		}
	}
}

func TestWriteBackSeekFailures(t *testing.T) {
	for _, failAt := range []int{4, 5} {
		v := customChild(t, []byte{1, 2}, 2)
		p := &seekFault{SeekableBuffer: kaitai.NewSeekableBuffer(nil), failAt: failAt}
		if err := v.Write(kaitai.NewWriter(p)); !errors.Is(err, io.ErrClosedPipe) {
			t.Fatalf("seek %d: %v", failAt, err)
		}
	}
}

type seekFault struct {
	*kaitai.SeekableBuffer
	calls, failAt int
}

func (p *seekFault) Seek(n int64, w int) (int64, error) {
	p.calls++
	if p.calls == p.failAt {
		return 0, io.ErrClosedPipe
	}
	return p.SeekableBuffer.Seek(n, w)
}

type failingParent struct {
	*kaitai.SeekableBuffer
	seek bool
}

func (p *failingParent) Write(b []byte) (int, error) { return 0, io.ErrClosedPipe }
func (p *failingParent) Seek(n int64, w int) (int64, error) {
	if p.seek {
		return 0, io.ErrClosedPipe
	}
	return p.SeekableBuffer.Seek(n, w)
}
