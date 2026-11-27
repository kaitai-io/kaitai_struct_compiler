package contract

import (
	"bytes"
	"errors"
	"math"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

type processedWritable interface {
	Check() error
	Write(*kaitai.Writer) error
}

func requireConsistency(t *testing.T, err error) {
	t.Helper()
	var consistency kaitai.ConsistencyError
	if !errors.As(err, &consistency) {
		t.Fatalf("error = %v; want ConsistencyError", err)
	}
}

func requireProcessedWrite(t *testing.T, value processedWritable, size int) []byte {
	t.Helper()
	if err := value.Check(); err != nil {
		t.Fatal(err)
	}
	buffer := kaitai.NewFixedSeekableBuffer(size)
	if err := value.Write(kaitai.NewWriter(buffer)); err != nil {
		t.Fatal(err)
	}
	return buffer.Bytes()
}

func TestProcessedRotateBytesCheck(t *testing.T) {
	valid := NewProcessedRotateBytes()
	valid.SetData([]byte("World"))
	requireProcessedWrite(t, valid, 5)

	badCheck := NewProcessedRotateBytes()
	badCheck.SetData([]byte("Way too long"))
	requireConsistency(t, badCheck.Check())
	var notChecked kaitai.ConsistencyNotCheckedError
	if err := badCheck.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(5))); !errors.As(err, &notChecked) {
		t.Fatalf("Write after failed Check = %v; want ConsistencyNotCheckedError", err)
	}
}

func TestProcessedRepeatedAndOptionalChecks(t *testing.T) {
	repeated := NewProcessedRotateRepeat()
	repeated.SetData([][]byte{{1, 2}, {3, 4, 5}})
	requireConsistency(t, repeated.Check())
	repeated.SetData([][]byte{{1, 2}, {3, 4}})
	requireProcessedWrite(t, repeated, 4)

	disabled := NewProcessedXorOptional(false)
	disabled.SetData([]byte{1, 2, 3})
	requireProcessedWrite(t, disabled, 0)

	enabled := NewProcessedXorOptional(true)
	enabled.SetData([]byte{1, 2, 3})
	requireConsistency(t, enabled.Check())
}

func TestProcessedSwitchBytesCheck(t *testing.T) {
	invalid := NewProcessedXorSwitch(2)
	invalid.SetData([]byte{1, 2, 3})
	requireConsistency(t, invalid.Check())

	valid := NewProcessedXorSwitch(2)
	valid.SetData([]byte{1, 2})
	requireProcessedWrite(t, valid, 2)
}

func TestProcessedUnsignedWidthsDoNotNarrowOrBecomeNegative(t *testing.T) {
	calculated := NewProcessedXorInstanceWidth()
	calculated.SetData(nil)
	requireConsistency(t, calculated.Check())

	direct := NewProcessedXorU8Width(math.MaxUint64)
	direct.SetData(make([]byte, math.MaxUint8))
	requireConsistency(t, direct.Check())

	enum := NewProcessedXorEnumWidth(ProcessedXorEnumWidth_Widths__Maximum)
	enum.SetData(make([]byte, math.MaxUint8))
	requireConsistency(t, enum.Check())
}

func TestProcessedPaddingChecksOnlyLogicalLength(t *testing.T) {
	valid := NewProcessedRotatePadded()
	valid.SetData([]byte{0xff, 1})
	requireProcessedWrite(t, valid, 4)

	invalid := NewProcessedRotatePadded()
	invalid.SetData([]byte{1, 2, 3, 4, 5})
	requireConsistency(t, invalid.Check())
}

var resizeCodecConstructs, resizeCodecEncodes int

type ResizeCodec struct{}

func NewResizeCodec() *ResizeCodec {
	resizeCodecConstructs++
	return &ResizeCodec{}
}

func (*ResizeCodec) Decode([]byte) ([]byte, error) { return []byte("expanded"), nil }

func (*ResizeCodec) Encode([]byte) ([]byte, error) {
	resizeCodecEncodes++
	return []byte{0xaa, 0xbb}, nil
}

func TestLengthChangingProcessorsStayAtEncodedLayer(t *testing.T) {
	resizeCodecConstructs, resizeCodecEncodes = 0, 0
	custom := NewProcessedResizeCustom()
	custom.SetData([]byte("expanded"))
	if err := custom.Check(); err != nil {
		t.Fatal(err)
	}
	if resizeCodecConstructs != 0 || resizeCodecEncodes != 0 {
		t.Fatalf("Check invoked custom processor: %d/%d", resizeCodecConstructs, resizeCodecEncodes)
	}
	if got := requireProcessedWrite(t, custom, 2); !bytes.Equal(got, []byte{0xaa, 0xbb}) {
		t.Fatalf("custom output = %x", got)
	}
	if resizeCodecConstructs != 1 || resizeCodecEncodes != 1 {
		t.Fatalf("Write processor calls = %d/%d; want 1/1", resizeCodecConstructs, resizeCodecEncodes)
	}

	decoded := []byte("compressible compressible compressible")
	encoded, err := kaitai.UnprocessZlib(decoded)
	if err != nil {
		t.Fatal(err)
	}
	if len(encoded) == len(decoded) {
		t.Fatalf("test requires different logical and encoded lengths: %d", len(encoded))
	}
	zlib := NewProcessedResizeZlib(uint32(len(encoded)))
	zlib.SetData(decoded)
	if got := requireProcessedWrite(t, zlib, len(encoded)); !bytes.Equal(got, encoded) {
		t.Fatalf("zlib output differs: %x want %x", got, encoded)
	}
}

func TestProcessedChildLengthRemainsAWriteTimeProperty(t *testing.T) {
	parent := NewProcessedRotateChild()
	parent.SetRoot_(parent)
	child := NewProcessedRotateChild_Body()
	child.SetParent_(parent)
	child.SetRoot_(parent)
	child.SetData([]byte{1, 2, 3})
	parent.SetChild(child)
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	if err := parent.Check(); err != nil {
		t.Fatalf("parent Check must not guess child encoded length: %v", err)
	}
	if err := parent.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(2))); err == nil {
		t.Fatal("processed child exceeding its encoded region was accepted")
	}

	child.SetData([]byte{1, 2})
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	if got := requireProcessedWrite(t, parent, 2); len(got) != 2 {
		t.Fatalf("child output length = %d", len(got))
	}
}
