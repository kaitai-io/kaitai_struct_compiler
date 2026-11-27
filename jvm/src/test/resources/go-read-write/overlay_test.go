package contract

import (
	"bytes"
	"io"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func writeOverlay(t *testing.T, size int, value interface {
	Check() error
	Write(*kaitai.Writer) error
}) []byte {
	t.Helper()
	if err := value.Check(); err != nil {
		t.Fatal(err)
	}
	output := kaitai.NewFixedSeekableBuffer(size)
	if err := value.Write(kaitai.NewWriter(output)); err != nil {
		t.Fatal(err)
	}
	return bytes.Clone(output.Bytes())
}

func TestOverlayEmptyPreservesCurrentParentBytes(t *testing.T) {
	input := []byte{0, 1, 2, 3, 4, 5, 6}
	p := NewOverlayEmpty()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if _, err := p.View(); err != nil {
		t.Fatal(err)
	}
	p.GetData()[1], p.GetData()[2] = 0xa2, 0xa3
	want := []byte{0, 0xa2, 0xa3, 3, 4, 5, 6}
	if got := writeOverlay(t, len(want), p); !bytes.Equal(got, want) {
		t.Fatalf("parsed write %x want %x", got, want)
	}

	fresh := NewOverlayEmpty()
	fresh.SetRoot_(fresh)
	fresh.SetData([]byte{10, 11, 12, 13, 14, 15})
	fresh.SetTail(16)
	view := NewOverlayEmpty_Empty()
	view.SetParent_(fresh)
	view.SetRoot_(fresh)
	if err := view.Check(); err != nil {
		t.Fatal(err)
	}
	fresh.SetView(view)
	want = []byte{10, 11, 12, 13, 14, 15, 16}
	if got := writeOverlay(t, len(want), fresh); !bytes.Equal(got, want) {
		t.Fatalf("fresh write %x want %x", got, want)
	}
}

func TestOverlayPartialOwnsEndpointsAndPreservesMutatedHoles(t *testing.T) {
	input := []byte{0, 1, 2, 3, 4, 5}
	p := NewOverlayPartial()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	view, err := p.View()
	if err != nil {
		t.Fatal(err)
	}
	if _, err = view.First(); err != nil {
		t.Fatal(err)
	}
	if _, err = view.Last(); err != nil {
		t.Fatal(err)
	}
	copy(p.GetData()[1:5], []byte{0xa1, 0xa2, 0xa3, 0xa4})
	view.SetFirst(0x11)
	view.SetLast(0x44)
	if err := view.Check(); err != nil {
		t.Fatal(err)
	}
	want := []byte{0, 0x11, 0xa2, 0xa3, 0x44, 5}
	first := writeOverlay(t, len(want), p)
	if !bytes.Equal(first, want) {
		t.Fatalf("partial write %x want %x", first, want)
	}

	copy(p.GetData()[1:5], []byte{0xb1, 0xb2, 0xb3, 0xb4})
	view.SetFirst(0x21)
	view.SetLast(0x54)
	if err := view.Check(); err != nil {
		t.Fatal(err)
	}
	want = []byte{0, 0x21, 0xb2, 0xb3, 0x54, 5}
	second := writeOverlay(t, len(want), p)
	if !bytes.Equal(second, want) {
		t.Fatalf("second write %x want %x", second, want)
	}
	if bytes.Equal(first, second) {
		t.Fatal("independent write reused stale overlay state")
	}
}

func TestOverlaySizeEosPreservesCurrentParentBytes(t *testing.T) {
	input := []byte{0, 1, 2, 3, 4, 5}
	p := NewOverlayEos()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if _, err := p.View(); err != nil {
		t.Fatal(err)
	}
	copy(p.GetData()[2:], []byte{0xa2, 0xa3, 0xa4, 0xa5})
	want := []byte{0, 1, 0xa2, 0xa3, 0xa4, 0xa5}
	if got := writeOverlay(t, len(want), p); !bytes.Equal(got, want) {
		t.Fatalf("size-eos write %x want %x", got, want)
	}
}

func TestOverlayExternalWriterPreservesBytesAndCursor(t *testing.T) {
	p := NewOverlayExternal(nil)
	p.SetRoot_(p)
	p.SetMarker(0x70)
	view := NewOverlayExternal_Empty()
	view.SetParent_(p)
	view.SetRoot_(p)
	if err := view.Check(); err != nil {
		t.Fatal(err)
	}
	p.SetView(view)
	external := kaitai.NewFixedSeekableBuffer(3)
	externalWriter := kaitai.NewWriter(external)
	if err := externalWriter.WriteBytes([]byte{0xaa, 0xbb, 0xcc}); err != nil {
		t.Fatal(err)
	}
	p.SetOutputWriter_(externalWriter)
	if got := writeOverlay(t, 1, p); !bytes.Equal(got, []byte{0x70}) {
		t.Fatalf("root write %x", got)
	}
	if !bytes.Equal(external.Bytes(), []byte{0xaa, 0xbb, 0xcc}) {
		t.Fatalf("external bytes %x", external.Bytes())
	}
	pos, err := externalWriter.Seek(0, io.SeekCurrent)
	if err != nil || pos != 3 {
		t.Fatalf("external cursor %d, %v", pos, err)
	}
}

func TestOverlayProcessedPositionedChildRemainsDense(t *testing.T) {
	input := []byte{1, 0xaa, 0xbb, 4}
	p := NewOverlayProcessed()
	if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
		t.Fatal(err)
	}
	if _, err := p.View(); err != nil {
		t.Fatal(err)
	}
	want := []byte{1, 0xff, 0xff, 4}
	if got := writeOverlay(t, len(want), p); !bytes.Equal(got, want) {
		t.Fatalf("dense processed write %x want %x", got, want)
	}
}
