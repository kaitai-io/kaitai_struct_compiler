package contract

import (
	"bytes"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"io"
	"strings"
	"testing"
)

type rawObject interface {
	Check() error
	Write(*kaitai.Writer) error
	SetData([]byte)
	GetData() []byte
}

func rawWrite(t *testing.T, p rawObject, logical, expected []byte, offset, position int) {
	t.Helper()
	p.SetData(logical)
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	for attempt := 0; attempt < 2; attempt++ {
		b := kaitai.NewFixedSeekableBuffer(len(expected))
		w := kaitai.NewWriter(b)
		if _, err := w.Seek(int64(offset), io.SeekStart); err != nil {
			t.Fatal(err)
		}
		if err := p.Write(w); err != nil {
			t.Fatal(err)
		}
		pos, err := w.Pos()
		if err != nil || pos != int64(position) || !bytes.Equal(b.Bytes(), expected) {
			t.Fatalf("bytes %x / cursor %d / error %v; want %x / %d", b.Bytes(), pos, err, expected, position)
		}
	}
}

func TestRawLiteral(t *testing.T) {
	p := NewRawPosition()
	if err := p.Read(kaitai.NewStream(bytes.NewReader([]byte{0x52})), nil, p); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(p.GetData(), []byte{0x53}) {
		t.Fatal(p.GetData())
	}
	rawWrite(t, p, p.GetData(), []byte{0x52}, 0, 1)
	rawWrite(t, p, []byte{0x54}, []byte{0x55}, 0, 1)
	rawWrite(t, NewRawPosition(), []byte{0x53}, []byte{0x52}, 0, 1)
}

func TestRawKnownLayouts(t *testing.T) {
	rawWrite(t, NewRawWidth(0), nil, nil, 0, 0)
	rawWrite(t, NewRawWidth(2), []byte{0x50, 0x51}, []byte{0x52, 0x53}, 0, 2)
	rawWrite(t, NewRawWidth(1), []byte{0x50}, []byte{0, 0, 0x53}, 2, 3)
	rawWrite(t, NewRawEos(), []byte{0x50, 0x51}, []byte{0, 0, 0x54, 0x55}, 2, 4)
	rawWrite(t, NewRawRotate(), []byte{0x29}, []byte{0x52}, 0, 1)
	rawWrite(t, NewRawLazy(), []byte{0x53}, []byte{0x52}, 0, 1)
	rawWrite(t, NewRawSize(), []byte{0x50}, []byte{0, 0x53, 0xff}, 1, 3)
	codecConstructs, codecEncodes = 0, 0
	rawWrite(t, NewRawCustom(), []byte{0x50}, []byte{0x52, 0x02}, 0, 2)
	if codecConstructs != 2 || codecEncodes != 2 {
		t.Fatalf("codec calls %d/%d", codecConstructs, codecEncodes)
	}
	rawWrite(t, NewRawEosCustom(), []byte{0x50}, []byte{0x53, 0x03, 0xff}, 0, 3)
	rawWrite(t, NewRawBoundedTermFalse(), []byte{0x50}, []byte{0x53, 0, 0xff}, 0, 3)
	rawWrite(t, NewRawBoundedTermTrue(), []byte{0x50, 3}, []byte{0x53, 0, 0xff}, 0, 3)
	rawWrite(t, NewRawTermStatic(), []byte{0x50}, []byte{0x51, 0}, 0, 2)
}

func TestRawKnownReads(t *testing.T) {
	width := NewRawWidth(2)
	s := kaitai.NewStream(bytes.NewReader([]byte{0x52, 0x53}))
	if err := width.Read(s, nil, width); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(width.GetData(), []byte{0x50, 0x51}) {
		t.Fatal(width.GetData())
	}
	eos := NewRawEos()
	s = kaitai.NewStream(bytes.NewReader([]byte{0, 0, 0x54, 0x55}))
	s.Seek(2, io.SeekStart)
	if err := eos.Read(s, nil, eos); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(eos.GetData(), []byte{0x50, 0x51}) {
		t.Fatal(eos.GetData())
	}
	rotate := NewRawRotate()
	s = kaitai.NewStream(bytes.NewReader([]byte{0x52}))
	if err := rotate.Read(s, nil, rotate); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(rotate.GetData(), []byte{0x29}) {
		t.Fatal(rotate.GetData())
	}
	custom := NewRawCustom()
	s = kaitai.NewStream(bytes.NewReader([]byte{0x52, 2}))
	if err := custom.Read(s, nil, custom); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(custom.GetData(), []byte{0x50}) {
		t.Fatal(custom.GetData())
	}
	bounded := NewRawBoundedTermTrue()
	s = kaitai.NewStream(bytes.NewReader([]byte{0x53, 0, 0xff}))
	if err := bounded.Read(s, nil, bounded); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(bounded.GetData(), []byte{0x50, 3}) {
		t.Fatal(bounded.GetData())
	}
	until := NewRawUntil()
	s = kaitai.NewStream(bytes.NewReader([]byte{0x51, 0x53}))
	if err := until.Read(s, nil, until); err != nil {
		t.Fatal(err)
	}
	if len(until.GetData()) != 2 || !bytes.Equal(until.GetData()[0], []byte{0x50}) || !bytes.Equal(until.GetData()[1], []byte{0x51}) {
		t.Fatal(until.GetData())
	}
}

var codecConstructs, codecEncodes int

type RawCodec struct{ key int64 }

func NewRawCodec(key int64) *RawCodec               { codecConstructs++; return &RawCodec{key} }
func (c *RawCodec) Decode(b []byte) ([]byte, error) { return []byte{b[0] ^ byte(c.key)}, nil }
func (c *RawCodec) Encode(b []byte) ([]byte, error) {
	codecEncodes++
	return []byte{b[0] ^ byte(c.key), byte(c.key)}, nil
}

var encodeError = errors.New("raw codec failed")
var failEncode bool

type FailingCodec struct{ key int64 }

func NewFailingCodec(key int64) *FailingCodec { codecConstructs++; return &FailingCodec{key} }
func (c *FailingCodec) Decode(b []byte) ([]byte, error) {
	return kaitai.ProcessXOR(b, []byte{byte(c.key)}), nil
}
func (c *FailingCodec) Encode(b []byte) ([]byte, error) {
	codecEncodes++
	if failEncode {
		return nil, encodeError
	}
	return kaitai.ProcessXOR(b, []byte{byte(c.key)}), nil
}

type MutateCodec struct{ key int64 }

func NewMutateCodec(key int64, widths []byte) *MutateCodec { widths[0] = 3; return &MutateCodec{key} }
func (c *MutateCodec) Decode(b []byte) ([]byte, error) {
	return kaitai.ProcessXOR(b, []byte{byte(c.key)}), nil
}
func (c *MutateCodec) Encode(b []byte) ([]byte, error) {
	return kaitai.ProcessXOR(b, []byte{byte(c.key)}), nil
}

func TestRawCapturedLayoutAndStreamAliases(t *testing.T) {
	widths := []byte{2}
	p := NewRawCapture(widths)
	p.SetData([]byte{0x50})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	b := kaitai.NewFixedSeekableBuffer(3)
	w := kaitai.NewWriter(b)
	if err := p.Write(w); err != nil {
		t.Fatal(err)
	}
	pos, _ := w.Pos()
	if pos != 2 || !bytes.Equal(b.Bytes(), []byte{0x52, 0xff, 0}) || widths[0] != 3 {
		t.Fatalf("captured layout %x / %d", b.Bytes(), pos)
	}
	ext := NewRawExternal(nil)
	ext.SetValue(0x70)
	ext.SetPart([]byte{0x50})
	external := kaitai.NewFixedSeekableBuffer(4)
	externalWriter := kaitai.NewWriter(external)
	externalWriter.Seek(3, io.SeekStart)
	ext.SetOutputWriter_(externalWriter)
	if err := ext.Check(); err != nil {
		t.Fatal(err)
	}
	root := kaitai.NewFixedSeekableBuffer(1)
	rootWriter := kaitai.NewWriter(root)
	if err := ext.Write(rootWriter); err != nil {
		t.Fatal(err)
	}
	rootPos, _ := rootWriter.Pos()
	extPos, _ := externalWriter.Pos()
	if rootPos != 1 || extPos != 3 || !bytes.Equal(root.Bytes(), []byte{0x70}) || !bytes.Equal(external.Bytes(), []byte{0, 0x53, 0, 0}) {
		t.Fatalf("external bytes %x, positions %d/%d", external.Bytes(), rootPos, extPos)
	}
	for _, alias := range []bool{false, true} {
		obj := NewRawParam(nil)
		obj.SetData([]byte{0x50})
		if err := obj.Check(); err != nil {
			t.Fatal(err)
		}
		output := kaitai.NewFixedSeekableBuffer(1)
		target := kaitai.NewWriter(output)
		arg := externalWriter
		expected := byte(0x53)
		if alias {
			arg = target
			expected = 0x51
		}
		obj.SetOutputWriter_(arg)
		if err := obj.Check(); err != nil {
			t.Fatal(err)
		}
		if err := obj.Write(target); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(output.Bytes(), []byte{expected}) {
			t.Fatalf("alias %v: %x", alias, output.Bytes())
		}
	}
}

func TestRawRepeatsBitsAndNested(t *testing.T) {
	p := NewRawRepeat()
	p.SetData([][]byte{{0x50}, {0x50}})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	b := kaitai.NewFixedSeekableBuffer(2)
	if err := p.Write(kaitai.NewWriter(b)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(b.Bytes(), []byte{0x51, 0x53}) {
		t.Fatalf("repeat %x", b.Bytes())
	}
	codecConstructs, codecEncodes = 0, 0
	custom := NewRawRepeatCustom()
	custom.SetData([][]byte{{0x50}, {0x50}})
	if err := custom.Check(); err != nil {
		t.Fatal(err)
	}
	b = kaitai.NewFixedSeekableBuffer(2)
	if err := custom.Write(kaitai.NewWriter(b)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(b.Bytes(), []byte{0x51, 0x53}) || codecConstructs != 2 || codecEncodes != 2 {
		t.Fatalf("repeat custom %x, calls %d/%d", b.Bytes(), codecConstructs, codecEncodes)
	}
	until := NewRawUntil()
	until.SetData([][]byte{{0x50}, {0x51}})
	if err := until.Check(); err != nil {
		t.Fatal(err)
	}
	b = kaitai.NewFixedSeekableBuffer(2)
	if err := until.Write(kaitai.NewWriter(b)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(b.Bytes(), []byte{0x51, 0x53}) {
		t.Fatalf("until %x", b.Bytes())
	}
	bits := NewRawBits()
	bits.SetPrefix(5)
	bits.SetData([]byte{0x50})
	bits.SetTail(17)
	if err := bits.Check(); err != nil {
		t.Fatal(err)
	}
	b = kaitai.NewFixedSeekableBuffer(3)
	if err := bits.Write(kaitai.NewWriter(b)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(b.Bytes(), []byte{0xa0, 0x52, 0x88}) {
		t.Fatalf("bits %x", b.Bytes())
	}
	nested := NewRawNested()
	if err := nested.Read(kaitai.NewStream(bytes.NewReader([]byte{0x7f, 0x55, 0x54})), nil, nested); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(nested.GetChild().GetData(), []byte{0x50, 0x51}) {
		t.Fatal(nested.GetChild().GetData())
	}
	if err := nested.Check(); err != nil {
		t.Fatal(err)
	}
	b = kaitai.NewFixedSeekableBuffer(3)
	if err := nested.Write(kaitai.NewWriter(b)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(b.Bytes(), []byte{0x7f, 0x55, 0x54}) {
		t.Fatalf("nested %x", b.Bytes())
	}
}

func TestRawTerminatorTiming(t *testing.T) {
	rawWrite(t, NewRawTermFalseTrueFalse(), []byte{0x50}, []byte{0x52, 0}, 0, 2)
	rawWrite(t, NewRawTermFalseFalseFalse(), []byte{0x50}, []byte{0x51, 0}, 0, 1)
	rawWrite(t, NewRawTermTrueTrueFalse(), []byte{0x50, 2}, []byte{0x52, 0}, 0, 2)
	rawWrite(t, NewRawTermTrueFalseFalse(), []byte{0x50, 1}, []byte{0x51, 0}, 0, 1)
	rawWrite(t, NewRawTermFalseTrueTrue(), []byte{0x50}, []byte{0x52, 0}, 0, 2)
	rawWrite(t, NewRawTermFalseTrueTrue(), []byte{0x50}, []byte{0x51}, 0, 1)
	rawWrite(t, NewRawTermFalseFalseTrue(), []byte{0x50}, []byte{0x51, 0}, 0, 1)
	rawWrite(t, NewRawTermFalseFalseTrue(), []byte{0x50}, []byte{0x51}, 0, 1)
	rawWrite(t, NewRawTermTrueTrueTrue(), []byte{0x50, 2}, []byte{0x52, 0}, 0, 2)
	rawWrite(t, NewRawTermTrueTrueTrue(), []byte{0x50, 0x51}, []byte{0x52, 0x53}, 0, 2)
}

func TestRawCircularLayoutDiagnostic(t *testing.T) {
	for _, p := range []rawObject{NewRawTermCustom(), NewRawTermTrueFalseTrue()} {
		p.SetData([]byte{0x50})
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		err := p.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(3)))
		if err == nil || !strings.Contains(err.Error(), "encoded layout") {
			t.Fatalf("expected explicit circular encoded layout error, got %v", err)
		}
	}
}

func TestRawOptionalLayoutRemainsCaptured(t *testing.T) {
	p := NewRawTermLayout()
	p.SetData([]byte{0x50})
	p.SetKey(7)
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	// The demanded key extends the stream. It must not change a captured absent
	// terminator into a present one after the processor used the shorter span.
	b := kaitai.NewSeekableBufferSize(1)
	err := p.Write(kaitai.NewWriter(b))
	if err == nil {
		t.Fatalf("accepted contradictory optional layout: %x", b.Bytes())
	}
}

func TestRawEosRequiresCapturedExtent(t *testing.T) {
	p := NewRawEos()
	p.SetData([]byte{0x50})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	b := kaitai.NewSeekableBuffer(nil)
	if err := p.Write(kaitai.NewWriter(b)); err == nil {
		t.Fatalf("inferred encoded extent from decoded payload: %x", b.Bytes())
	}
}

func TestRawFailureAndSelection(t *testing.T) {
	codecConstructs, codecEncodes = 0, 0
	disabled := NewRawChoice(false)
	if err := disabled.Check(); err != nil {
		t.Fatal(err)
	}
	if err := disabled.Write(kaitai.NewWriter(new(bytes.Buffer))); err != nil {
		t.Fatal(err)
	}
	if codecConstructs != 0 || codecEncodes != 0 {
		t.Fatal("disabled processor evaluated")
	}
	failEncode = true
	p := NewRawFailure()
	p.SetData([]byte{0x50})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	b := kaitai.NewFixedSeekableBuffer(4)
	w := kaitai.NewWriter(b)
	w.Seek(2, io.SeekStart)
	err := p.Write(w)
	failEncode = false
	pos, _ := w.Pos()
	if !errors.Is(err, encodeError) || pos != 2 || !bytes.Equal(b.Bytes(), []byte{0, 0, 0, 0}) {
		t.Fatalf("failed write %v / %d / %x", err, pos, b.Bytes())
	}
	rawWrite(t, p, []byte{0x50}, []byte{0, 0, 0x53, 0}, 2, 3)
	selected := NewRawChoice(true)
	selected.SetData([]byte{0x50})
	if err := selected.Check(); err != nil {
		t.Fatal(err)
	}
	f := &rawFaultBuffer{SeekableBuffer: kaitai.NewFixedSeekableBuffer(1), failEnd: true}
	if err := selected.Write(kaitai.NewWriter(f)); err != nil {
		t.Fatalf("unselected size branch evaluated: %v", err)
	}
	if !bytes.Equal(f.Bytes(), []byte{0x51}) {
		t.Fatal(f.Bytes())
	}
	for _, tag := range []uint8{0, 1} {
		codecConstructs, codecEncodes = 0, 0
		selected := NewRawSwitch(tag)
		var value interface{} = []byte{0x50}
		want := byte(0x51)
		if tag == 1 {
			value = uint8(0x70)
			want = 0x70
		}
		selected.SetData(value)
		if err := selected.Check(); err != nil {
			t.Fatal(err)
		}
		b := kaitai.NewFixedSeekableBuffer(1)
		if err := selected.Write(kaitai.NewWriter(b)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(b.Bytes(), []byte{want}) {
			t.Fatalf("switch %d: %x", tag, b.Bytes())
		}
		calls := 1 - int(tag)
		if codecConstructs != calls || codecEncodes != calls {
			t.Fatalf("switch calls %d/%d", codecConstructs, codecEncodes)
		}
	}
	bits := NewRawErrorBits()
	bits.SetPrefix(5)
	bits.SetData([]byte{0x50})
	if err := bits.Check(); err != nil {
		t.Fatal(err)
	}
	b = kaitai.NewFixedSeekableBuffer(2)
	w = kaitai.NewWriter(b)
	failEncode = true
	err = bits.Write(w)
	failEncode = false
	pos, _ = w.Pos()
	if !errors.Is(err, encodeError) || pos != 1 || !bytes.Equal(b.Bytes(), []byte{0xa0, 0}) {
		t.Fatalf("byte boundary not retained: %v/%d/%x", err, pos, b.Bytes())
	}
}

var seekError = errors.New("raw seek failed")
var restoreError = errors.New("raw restore failed")
var dataError = errors.New("raw output failed")

type rawFaultBuffer struct {
	*kaitai.SeekableBuffer
	failCurrent, failMove, failRestore, failEnd, failWrite, partialWrite bool
}

func (b *rawFaultBuffer) Seek(offset int64, whence int) (int64, error) {
	if b.failCurrent && whence == io.SeekCurrent {
		b.failCurrent = false
		return 0, seekError
	}
	pos, err := b.SeekableBuffer.Seek(offset, whence)
	if err != nil {
		return pos, err
	}
	if b.failMove && whence == io.SeekStart && offset == 3 {
		b.failMove = false
		return pos, seekError
	}
	if b.failRestore && whence == io.SeekStart && offset == 2 {
		b.failRestore = false
		return pos, restoreError
	}
	if b.failEnd && whence == io.SeekEnd {
		b.failEnd = false
		return pos, seekError
	}
	return pos, nil
}
func (b *rawFaultBuffer) Write(p []byte) (int, error) {
	if b.failWrite {
		return 0, dataError
	}
	if b.partialWrite && len(p) > 0 {
		n, _ := b.SeekableBuffer.Write(p[:1])
		return n, dataError
	}
	return b.SeekableBuffer.Write(p)
}
func TestRawFaultRestoration(t *testing.T) {
	for _, tc := range []struct {
		name                                 string
		current, move, restore, codec, write bool
		causes                               []error
	}{
		{name: "initial position", current: true, causes: []error{seekError}},
		{name: "seek moves then errors", move: true, causes: []error{seekError}},
		{name: "restore only", restore: true, causes: []error{restoreError}},
		{name: "seek and restore", move: true, restore: true, causes: []error{seekError, restoreError}},
		{name: "codec and restore", codec: true, restore: true, causes: []error{encodeError, restoreError}},
		{name: "final data write", write: true, causes: []error{dataError}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			p := NewRawFailure()
			p.SetData([]byte{0x50})
			if err := p.Check(); err != nil {
				t.Fatal(err)
			}
			b := &rawFaultBuffer{SeekableBuffer: kaitai.NewFixedSeekableBuffer(4), failCurrent: tc.current, failMove: tc.move, failRestore: tc.restore, failWrite: tc.write}
			b.SeekableBuffer.Seek(2, io.SeekStart)
			failEncode = tc.codec
			err := p.Write(kaitai.NewWriter(b))
			failEncode = false
			for _, cause := range tc.causes {
				if !errors.Is(err, cause) {
					t.Fatalf("missing cause %v: %v", cause, err)
				}
			}
			pos, _ := b.SeekableBuffer.Seek(0, io.SeekCurrent)
			if pos != 2 || !bytes.Equal(b.Bytes(), []byte{0, 0, 0, 0}) {
				t.Fatalf("restoration %d/%x", pos, b.Bytes())
			}
			rawWrite(t, p, []byte{0x50}, []byte{0x51}, 0, 1)
		})
	}
	for _, p := range []rawObject{NewRawEos(), NewRawKeySize()} {
		p.SetData([]byte{0x50})
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		b := &rawFaultBuffer{SeekableBuffer: kaitai.NewFixedSeekableBuffer(4), failEnd: true}
		b.SeekableBuffer.Seek(2, io.SeekStart)
		err := p.Write(kaitai.NewWriter(b))
		pos, _ := b.SeekableBuffer.Seek(0, io.SeekCurrent)
		if !errors.Is(err, seekError) || pos != 2 {
			t.Fatalf("end query/key error restoration: %v/%d", err, pos)
		}
	}
	for _, width := range []int64{-1, 9223372036854775807} {
		p := NewRawBadSize(width)
		p.SetData(nil)
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		b := kaitai.NewFixedSeekableBuffer(2)
		w := kaitai.NewWriter(b)
		w.Seek(1, io.SeekStart)
		err := p.Write(w)
		pos, _ := w.Pos()
		if err == nil || pos != 1 || !bytes.Equal(b.Bytes(), []byte{0, 0}) {
			t.Fatalf("invalid layout accepted: %v/%d/%x", err, pos, b.Bytes())
		}
	}
	p := NewRawWidth(2)
	p.SetData([]byte{0x50, 0x51})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	b := &rawFaultBuffer{SeekableBuffer: kaitai.NewFixedSeekableBuffer(2), partialWrite: true}
	err := p.Write(kaitai.NewWriter(b))
	pos, _ := b.SeekableBuffer.Seek(0, io.SeekCurrent)
	if !errors.Is(err, dataError) || pos != 1 || !bytes.Equal(b.Bytes(), []byte{0x52, 0}) {
		t.Fatalf("partial write %v/%d/%x", err, pos, b.Bytes())
	}
}
