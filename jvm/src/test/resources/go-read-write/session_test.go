package contract

import (
	"bytes"
	"encoding/hex"
	"errors"
	"fmt"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"io"
	"testing"
)

type sessionObject interface {
	Check() error
	FetchInstances() error
	Write(*kaitai.Writer) error
}

func sessionBytes(t *testing.T, obj sessionObject, want []byte) []byte {
	t.Helper()
	if err := obj.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewFixedSeekableBuffer(len(want))
	if err := obj.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), want) {
		t.Fatalf("%T: got %x want %x", obj, out.Bytes(), want)
	}
	return out.Bytes()
}
func unhex(s string) []byte {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return b
}

func TestSessionForwardSiblingParsedAndFresh(t *testing.T) {
	input := unhex("0300000000000000ffffffff04000000fffffffe0a000000fffffffd746865007261696e790064617920697420697300")
	parsed := NewInstanceIoUser()
	if err := parsed.Read(kaitai.NewStream(bytes.NewReader(input)), nil, parsed); err != nil {
		t.Fatal(err)
	}
	if err := parsed.FetchInstances(); err != nil {
		t.Fatal(err)
	}
	fresh := NewInstanceIoUser()
	fresh.SetRoot_(fresh)
	fresh.SetQtyEntries(3)
	strings := NewInstanceIoUser_StringsObj()
	strings.SetParent_(fresh)
	strings.SetRoot_(fresh)
	strings.SetStr([]string{"the", "rainy", "day it is"})
	fresh.SetStrings(strings)
	entries := []*InstanceIoUser_Entry{}
	for i, name := range []string{"the", "rainy", "day it is"} {
		e := NewInstanceIoUser_Entry()
		e.SetParent_(fresh)
		e.SetRoot_(fresh)
		e.SetNameOfs([]uint32{0, 4, 10}[i])
		e.SetValue([]uint32{0xffffffff, 0xfeffffff, 0xfdffffff}[i])
		e.SetName(name)
		if err := e.Check(); err != nil {
			t.Fatal(err)
		}
		entries = append(entries, e)
	}
	fresh.SetEntries(entries)
	if err := strings.Check(); err != nil {
		t.Fatal(err)
	}
	for _, p := range []*InstanceIoUser{parsed, fresh} {
		first := sessionBytes(t, p, input)
		p.GetEntries()[0].SetName("THE")
		if err := p.GetEntries()[0].Check(); err != nil {
			t.Fatal(err)
		}
		want := bytes.Clone(input)
		copy(want[28:], "THE")
		sessionBytes(t, p, want)
		if !bytes.Equal(first, input) {
			t.Fatal("second write changed first output")
		}
		if p._activeWriter != nil || p._writeContext != nil || p.Writer_() == nil {
			t.Fatal("binding lifetime")
		}
		again := NewInstanceIoUser()
		if err := again.Read(kaitai.NewStream(bytes.NewReader(want)), nil, again); err != nil {
			t.Fatal(err)
		}
		if name, err := again.GetEntries()[0].Name(); err != nil || name != "THE" {
			t.Fatalf("name %q %v", name, err)
		}
	}
}

func TestSessionEarlierNestedAndZeroLengthInstance(t *testing.T) {
	input := unhex("53666f6f00584200000059feca00004907")
	p := NewInstanceIoUserEarlier()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if err := p.FetchInstances(); err != nil {
		t.Fatal(err)
	}
	fresh := NewInstanceIoUserEarlier()
	fresh.SetRoot_(fresh)
	slot := func(parent kaitai.Struct, content, last byte) *InstanceIoUserEarlier_Slot {
		s := NewInstanceIoUserEarlier_Slot()
		s.SetRoot_(fresh)
		s.SetParent_(parent)
		s.SetContent(content)
		s.SetLast(last)
		if err := s.Check(); err != nil {
			t.Fatal(err)
		}
		return s
	}
	fresh.SetSizedA(slot(fresh, 0x53, 0x58))
	fresh.SetSizedB(slot(fresh, 0x42, 0xfe))
	foo := func(indicator, content, last, bar byte) *InstanceIoUserEarlier_Foo {
		f := NewInstanceIoUserEarlier_Foo()
		f.SetRoot_(fresh)
		f.SetParent_(fresh)
		f.SetIndicator(indicator)
		f.SetBar(bar)
		f.SetInst(slot(f, content, last))
		if err := f.Check(); err != nil {
			t.Fatal(err)
		}
		return f
	}
	fresh.SetIntoB(foo(0xca, 0, 0x59, 0))
	fresh.SetIntoASkipped(foo(0, 0, 0, 0))
	fresh.SetIntoA(foo(0, 0x66, 0, 0x49))
	b := NewInstanceIoUserEarlier_Baz()
	b.SetRoot_(fresh)
	b.SetParent_(fresh)
	b.SetV(7)
	if err := b.Check(); err != nil {
		t.Fatal(err)
	}
	fresh.SetLastAccessor(b)
	fresh.SetAMid(0x6f6f)
	fresh.SetBMid(0)
	for _, obj := range []*InstanceIoUserEarlier{p, fresh} {
		first := sessionBytes(t, obj, input)
		skipped, err := obj.GetIntoASkipped().Inst()
		if err != nil {
			t.Fatal(err)
		}
		if size, err := skipped.Writer_().Writer.(io.Seeker).Seek(0, io.SeekEnd); err != nil || size != 0 {
			t.Fatalf("zero-sized instance became %d: %v", size, err)
		}
		obj.SetAMid(0x4142)
		want := bytes.Clone(input)
		want[2], want[3] = 0x42, 0x41
		sessionBytes(t, obj, want)
		if !bytes.Equal(first, input) {
			t.Fatal("mutated prior output")
		}
	}
}

var codecConstructions int
var failSessionEncode bool

type MyCustomFx struct{ key byte }

func NewMyCustomFx[T ~int | ~int64 | ~uint64](key T, flag bool, ignored []byte) *MyCustomFx {
	codecConstructions++
	k := byte(key)
	if !flag {
		k = -k
	}
	return &MyCustomFx{k}
}
func (f *MyCustomFx) Decode(src []byte) ([]byte, error) {
	for i := range src {
		src[i] += f.key
	}
	return src, nil
}
func (f *MyCustomFx) Encode(src []byte) ([]byte, error) {
	if failSessionEncode {
		failSessionEncode = false
		return nil, io.ErrClosedPipe
	}
	for i := range src {
		src[i] -= f.key
	}
	return src, nil
}

func TestSessionRepeatedXorParsedFreshAndMutated(t *testing.T) {
	input := unhex("ecbba3148ad4cc348edad1")
	p := NewProcessRepeatUsertypeDynargXor()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if err := p.FetchInstances(); err != nil {
		t.Fatal(err)
	}
	f := NewProcessRepeatUsertypeDynargXor()
	f.SetRoot_(f)
	var blocks []*ProcessRepeatUsertypeDynargXor_Block
	for _, a := range []uint32{0x8a3d2572, 0x0fb54d55} {
		b := NewProcessRepeatUsertypeDynargXor_Block()
		b.SetRoot_(f)
		b.SetParent_(f)
		b.SetA(a)
		if err := b.Check(); err != nil {
			t.Fatal(err)
		}
		blocks = append(blocks, b)
	}
	f.SetBlocks(blocks)
	w := NewProcessRepeatUsertypeDynargXor_BlocksBWrapper()
	w.SetRoot_(f)
	w.SetParent_(f)
	w.SetDummy(0xd1)
	w.SetBlocks0B(0x14)
	w.SetBlocks1B(0x5b)
	if err := w.Check(); err != nil {
		t.Fatal(err)
	}
	f.SetBlocksB(w)
	for _, obj := range []*ProcessRepeatUsertypeDynargXor{p, f} {
		first := sessionBytes(t, obj, input)
		obj.GetBlocksB().SetBlocks0B(0x15)
		if err := obj.GetBlocksB().Check(); err != nil {
			t.Fatal(err)
		}
		want := bytes.Clone(input)
		want[4] = 0x8b
		sessionBytes(t, obj, want)
		if !bytes.Equal(first, input) {
			t.Fatal("prior output changed")
		}
	}
}

func TestSessionRepeatedCustomConstructsOnceAndRecovers(t *testing.T) {
	input := unhex("09ac8d8dedba7b93632301")
	p := NewProcessRepeatUsertypeDynargCustom()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if err := p.FetchInstances(); err != nil {
		t.Fatal(err)
	}
	f := NewProcessRepeatUsertypeDynargCustom()
	f.SetRoot_(f)
	f.Blocks_InnerSize = []int{5, 5}
	var blocks []*ProcessRepeatUsertypeDynargCustom_Block
	for _, a := range []uint32{0x8888a704, 0x7aaa92d1} {
		b := NewProcessRepeatUsertypeDynargCustom_Block()
		b.SetRoot_(f)
		b.SetParent_(f)
		b.SetA(a)
		if err := b.Check(); err != nil {
			t.Fatal(err)
		}
		blocks = append(blocks, b)
	}
	f.SetBlocks(blocks)
	w := NewProcessRepeatUsertypeDynargCustom_BlocksBWrapper()
	w.SetRoot_(f)
	w.SetParent_(f)
	w.SetDummy(1)
	w.SetBlocks0B(0xe8)
	w.SetBlocks1B(0x3a)
	if err := w.Check(); err != nil {
		t.Fatal(err)
	}
	f.SetBlocksB(w)
	for _, obj := range []*ProcessRepeatUsertypeDynargCustom{p, f} {
		if err := obj.Check(); err != nil {
			t.Fatal(err)
		}
		failSessionEncode = true
		codecConstructions = 0
		if err := obj.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(11))); !errors.Is(err, io.ErrClosedPipe) {
			t.Fatalf("encode error %v", err)
		}
		if codecConstructions != 2 || obj._activeWriter != nil || obj.GetBlocks()[0]._activeWriter != nil {
			t.Fatal("failure leaked context or reconstructed codecs")
		}
		codecConstructions = 0
		first := sessionBytes(t, obj, input)
		if codecConstructions != 2 {
			t.Fatalf("constructed %d", codecConstructions)
		}
		obj.GetBlocksB().SetBlocks1B(0x3b)
		if err := obj.GetBlocksB().Check(); err != nil {
			t.Fatal(err)
		}
		want := bytes.Clone(input)
		want[9] = 0x24
		sessionBytes(t, obj, want)
		if !bytes.Equal(first, input) {
			t.Fatal("prior bytes changed")
		}
	}
}

func TestSessionRepeatedRotateParsedFreshAndMutated(t *testing.T) {
	input := unhex("09ac8d8dedba7b936323012a34b239b2")
	p := NewProcessRepeatUsertypeDynargRotate()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if err := p.FetchInstances(); err != nil {
		t.Fatal(err)
	}
	f := NewProcessRepeatUsertypeDynargRotate()
	f.SetRoot_(f)
	makeBlocks := func(values []uint16) []*ProcessRepeatUsertypeDynargRotate_Block {
		var out []*ProcessRepeatUsertypeDynargRotate_Block
		for _, a := range values {
			b := NewProcessRepeatUsertypeDynargRotate_Block()
			b.SetRoot_(f)
			b.SetParent_(f)
			b.SetA(a)
			if err := b.Check(); err != nil {
				t.Fatal(err)
			}
			out = append(out, b)
		}
		return out
	}
	f.SetBlocksRol(makeBlocks([]uint16{0x6548, 0xb736}))
	f.SetBlocksRor(makeBlocks([]uint16{0x726f, 0x40c8, 0x591a}))
	w := NewProcessRepeatUsertypeDynargRotate_BlocksBWrapper()
	w.SetRoot_(f)
	w.SetParent_(f)
	w.SetDummy(0xb2)
	w.SetBlocksRol0B(0x6c)
	w.SetBlocksRol1B(0xea)
	w.SetBlocksRor0B(0x6c)
	w.SetBlocksRor1B(0x8a)
	w.SetBlocksRor2B(0x9c)
	if err := w.Check(); err != nil {
		t.Fatal(err)
	}
	f.SetBlocksB(w)
	for _, obj := range []*ProcessRepeatUsertypeDynargRotate{p, f} {
		first := sessionBytes(t, obj, input)
		obj.GetBlocksB().SetBlocksRol0B(0x64)
		if err := obj.GetBlocksB().Check(); err != nil {
			t.Fatal(err)
		}
		want := bytes.Clone(input)
		want[2] = 0x8c
		sessionBytes(t, obj, want)
		if !bytes.Equal(first, input) {
			t.Fatal("prior bytes changed")
		}
	}
}

type sessionHook struct {
	*kaitai.SeekableBuffer
	hook func()
}

func (h *sessionHook) Write(data []byte) (int, error) {
	if h.hook != nil {
		f := h.hook
		h.hook = nil
		f()
	}
	return h.SeekableBuffer.Write(data)
}
func TestSessionSnapshotsSliceKeyAndRejectsForeignContext(t *testing.T) {
	key := []byte{0x20}
	p := NewSessionKey(key)
	p.SetRoot_(p)
	b := NewSessionKey_Body()
	b.SetRoot_(p)
	b.SetParent_(p)
	b.SetValue(0x61)
	if err := b.Check(); err != nil {
		t.Fatal(err)
	}
	p.SetChild(b)
	p.SetTail(0x42)
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	out := &sessionHook{SeekableBuffer: kaitai.NewSeekableBuffer(nil), hook: func() { key[0] = 0x30 }}
	if err := p.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0x41, 0x42}) {
		t.Fatalf("key was not snapshotted %x", out.Bytes())
	}
	if _, err := p.WriterForContext_(nil); err == nil {
		t.Fatal("accepted missing context")
	}
	context := kaitai.NewWriteContext()
	defer context.Close()
	foreign := kaitai.NewWriteContext()
	defer foreign.Close()
	if err := p.WriteWithContext_(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil)), context); err != nil {
		t.Fatal(err)
	}
	if _, err := p.WriterForContext_(foreign); err == nil {
		t.Fatal("retained writer accepted for foreign session")
	}
	if err := p.WriteWithContext_(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil)), context); err == nil {
		t.Fatal("duplicate occurrence accepted")
	}
}

func TestSessionOptionalPeekLayoutAndLateTerminalPatch(t *testing.T) {
	for _, present := range []bool{false, true} {
		p := NewWriteOptionalIncludedPeek()
		p.SetRoot_(p)
		b := NewWriteOptionalIncludedPeek_Body()
		b.SetRoot_(p)
		b.SetParent_(p)
		data := []byte{0x41, 0x42}
		if present {
			data[1] = 0
		}
		b.SetData(data)
		if err := b.Check(); err != nil {
			t.Fatal(err)
		}
		p.SetChild(b)
		p.Child_OuterSize = 2
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		if err := p.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err == nil {
			t.Fatal("unset layout accepted")
		}
		p.SetChildTerminatorPresent_(present)
		sessionBytes(t, p, data)
		p.SetChildTerminatorPresent_(!present)
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		if err := p.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err == nil {
			t.Fatal("contradictory layout accepted")
		}
	}
	p := NewSessionOptional()
	p.SetRoot_(p)
	p.SetTail(0)
	b := NewSessionOptional_Body()
	b.SetRoot_(p)
	b.SetParent_(p)
	b.SetValue(0x41)
	b.SetTerminal(0)
	if err := b.Check(); err != nil {
		t.Fatal(err)
	}
	p.SetChild(b)
	p.Child_OuterSize = 2
	p.SetChildTerminatorPresent_(true)
	sessionBytes(t, p, []byte{0x41, 0})
	b.SetTerminal(1)
	if err := b.Check(); err != nil {
		t.Fatal(err)
	}
	if err := p.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err == nil {
		t.Fatal("late patch contradicted declared terminator")
	}
}

func TestSessionRepeatedOptionalMetadataResetsOnRead(t *testing.T) {
	p := NewSessionRepeatOptional()
	for _, input := range [][]byte{{0x41, 0}, {0x41, 0x42}} {
		p.SetChildren(nil)
		p.Children_OuterSize = nil
		p._raw_Children = nil
		if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
			t.Fatal(err)
		}
		sessionBytes(t, p, input)
	}
	p.SetChildrenTerminatorPresent_([]bool{false, false})
	sessionBytes(t, p, []byte{0x41, 0x42})
	p.SetChildrenTerminatorPresent_([]bool{false})
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	if err := p.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err == nil {
		t.Fatal("missing repeated metadata accepted")
	}
}

func TestSessionExternalParameterOnlyFlushesNewReservations(t *testing.T) {
	external := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(external)
	old, err := w.NewChild(1, 1, nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := old.WriteU1(0xee); err != nil {
		t.Fatal(err)
	}
	p := NewSessionExternal(nil)
	p.SetRoot_(p)
	p.SetValue(0x42)
	p.SetOutputWriter_(w)
	b := NewSessionExternal_Body()
	b.SetRoot_(p)
	b.SetParent_(p)
	b.SetValue(0x61)
	if err := b.Check(); err != nil {
		t.Fatal(err)
	}
	p.SetPart(b)
	sessionBytes(t, p, []byte{0x42})
	if !bytes.Equal(external.Bytes(), []byte{0x61}) {
		t.Fatalf("external child %x", external.Bytes())
	}
	// A historical handle remains unflushed and may be explicitly finalized.
	if err := old.WriteBack(); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(external.Bytes(), []byte{0xee}) {
		t.Fatalf("old child %x", external.Bytes())
	}
}

func TestSessionNestedPendingOperationPrecedesFollowingOverlap(t *testing.T) {
	p := NewSessionOrder()
	p.SetRoot_(p)
	p.SetValue(0)
	p.SetB(0x22)
	a := NewSessionOrder_Body()
	a.SetRoot_(p)
	a.SetParent_(p)
	a.SetMark(0x11)
	if err := a.Check(); err != nil {
		t.Fatal(err)
	}
	p.SetA(a)
	// The body is first discovered while completing root instance a. Its mark
	// must finish before root instance b overwrites the same byte. A FIFO of
	// object callbacks would incorrectly produce 0x11 instead of 0x22.
	sessionBytes(t, p, []byte{0x22})
}

type sessionFailureBuffer struct {
	*kaitai.SeekableBuffer
	fail bool
}

func (b *sessionFailureBuffer) Write(data []byte) (int, error) {
	if b.fail {
		return 0, io.ErrClosedPipe
	}
	return b.SeekableBuffer.Write(data)
}

func sessionOrderPending(t *testing.T) *SessionOrder {
	t.Helper()
	p := NewSessionOrder()
	p.SetRoot_(p)
	p.SetValue(0)
	p.SetB(0x22)
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	return p
}

func TestOutstandingSessionsRejectSecondBindingForEveryFinishAndCloseOrder(t *testing.T) {
	for _, secondFinishesFirst := range []bool{false, true} {
		for _, secondClosesFirst := range []bool{false, true} {
			for _, failFirst := range []bool{false, true} {
				t.Run(fmt.Sprintf("finishSecondFirst=%v/closeSecondFirst=%v/failFirst=%v", secondFinishesFirst, secondClosesFirst, failFirst), func(t *testing.T) {
					p := sessionOrderPending(t)
					one, two := kaitai.NewWriteContext(), kaitai.NewWriteContext()
					defer one.Close()
					defer two.Close()
					out1 := &sessionFailureBuffer{SeekableBuffer: kaitai.NewSeekableBuffer(nil)}
					out2 := kaitai.NewSeekableBuffer(nil)
					writer1, writer2 := kaitai.NewWriter(out1), kaitai.NewWriter(out2)
					if err := p.WriteWithContext_(writer1, one); err != nil {
						t.Fatal(err)
					}
					var consistency kaitai.ConsistencyError
					if err := p.WriteWithContext_(writer2, two); !errors.As(err, &consistency) {
						t.Fatalf("second outstanding binding must be rejected: %v", err)
					}
					if p._writeContext != one || p._activeWriter != writer1 || p.Writer_() != writer1 || !p._w_b {
						t.Fatal("rejected binding changed first session state")
					}
					finishFirst := func() {
						out1.fail = failFirst
						err := one.Finish()
						if failFirst {
							if !errors.Is(err, io.ErrClosedPipe) {
								t.Fatalf("first failure: %v", err)
							}
						} else if err != nil {
							t.Fatal(err)
						}
					}
					finishSecond := func() {
						if err := two.Finish(); err != nil {
							t.Fatal(err)
						}
					}
					if secondFinishesFirst {
						finishSecond()
						finishFirst()
					} else {
						finishFirst()
						finishSecond()
					}
					want := []byte{0x22}
					if failFirst {
						want[0] = 0
					}
					if !bytes.Equal(out1.Bytes(), want) || len(out2.Bytes()) != 0 {
						t.Fatalf("crossed outputs first=%x second=%x", out1.Bytes(), out2.Bytes())
					}
					if secondClosesFirst {
						two.Close()
						one.Close()
					} else {
						one.Close()
						two.Close()
					}
					if p._writeContext != nil || p._activeWriter != nil {
						t.Fatal("close resurrected a context")
					}
					if _, err := p.WriterForContext_(one); err == nil {
						t.Fatal("closed context still owns receiver")
					}
					sessionBytes(t, p, []byte{0x22})
					if !bytes.Equal(out1.Bytes(), want) || len(out2.Bytes()) != 0 {
						t.Fatal("fresh retry changed old output")
					}
				})
			}
		}
	}
}

func TestIndependentSynchronousWriteRestoresOutstandingSession(t *testing.T) {
	for _, failInner := range []bool{false, true} {
		p := sessionOrderPending(t)
		one := kaitai.NewWriteContext()
		defer one.Close()
		out1 := kaitai.NewSeekableBuffer(nil)
		writer1 := kaitai.NewWriter(out1)
		if err := p.WriteWithContext_(writer1, one); err != nil {
			t.Fatal(err)
		}
		out2 := &sessionFailureBuffer{SeekableBuffer: kaitai.NewSeekableBuffer(nil), fail: failInner}
		err := p.Write(kaitai.NewWriter(out2))
		if failInner {
			if !errors.Is(err, io.ErrClosedPipe) {
				t.Fatalf("inner failure: %v", err)
			}
		} else if err != nil {
			t.Fatal(err)
		}
		if p._writeContext != one || p._activeWriter != writer1 || p.Writer_() != writer1 || !p._w_b {
			t.Fatal("synchronous write lost suspended binding")
		}
		if err := one.Finish(); err != nil {
			t.Fatal(err)
		}
		one.Close()
		if !bytes.Equal(out1.Bytes(), []byte{0x22}) {
			t.Fatalf("first output %x", out1.Bytes())
		}
		if !failInner && !bytes.Equal(out2.Bytes(), []byte{0x22}) {
			t.Fatalf("second output %x", out2.Bytes())
		}
		if p._writeContext != nil || p._activeWriter != nil {
			t.Fatal("active binding remained after close")
		}
	}
}

func TestIndependentSynchronousWriteRestoresNestedBindingsAndPendingBits(t *testing.T) {
	p := NewSessionBits()
	p.SetRoot_(p)
	p.SetTail(5)
	child := NewSessionBits_Body()
	child.SetRoot_(p)
	child.SetParent_(p)
	child.SetValue(0x42)
	p.SetChild(child)
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	if err := p.Check(); err != nil {
		t.Fatal(err)
	}
	one := kaitai.NewWriteContext()
	defer one.Close()
	out1, out2 := kaitai.NewSeekableBuffer(nil), kaitai.NewSeekableBuffer(nil)
	writer1 := kaitai.NewWriter(out1)
	if err := p.WriteWithContext_(writer1, one); err != nil {
		t.Fatal(err)
	}
	childWriter1 := child.Writer_()
	if err := p.Write(kaitai.NewWriter(out2)); err != nil {
		t.Fatal(err)
	}
	if p._writeContext != one || p._activeWriter != writer1 || child._writeContext != one || child._activeWriter != childWriter1 {
		t.Fatal("independent nested write did not restore suspended graph")
	}
	if err := one.Finish(); err != nil {
		t.Fatal(err)
	}
	one.Close()
	if !bytes.Equal(out1.Bytes(), []byte{0x42}) || !bytes.Equal(out2.Bytes(), []byte{0x42, 0xa0}) {
		t.Fatalf("nested outputs %x/%x", out1.Bytes(), out2.Bytes())
	}
	if err := writer1.WriteBitsIntBe(5, 19); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out1.Bytes(), []byte{0x42, 0xb3}) {
		t.Fatalf("outer pending bits %x", out1.Bytes())
	}
}
