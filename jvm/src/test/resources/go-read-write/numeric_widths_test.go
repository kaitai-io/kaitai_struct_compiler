package contract

import (
	"bytes"
	"io"
	"reflect"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

type positionCounter struct {
	*bytes.Reader
	positions int
}

func (r *positionCounter) Seek(offset int64, whence int) (int64, error) {
	if offset == 0 && whence == io.SeekCurrent {
		r.positions++
	}
	return r.Reader.Seek(offset, whence)
}

func TestNumericWidths(t *testing.T) {
	// -2, 300, 2^32-1, 2^63+5, indexed children 0/1, encoded bytes, LE switch.
	input := []byte{254, 44, 1, 255, 255, 255, 255, 5, 0, 0, 0, 0, 0, 0, 128, 0, 1, 82, 98, 65, 67}
	p := NewNumericWidths(-128)
	reader := &positionCounter{Reader: bytes.NewReader(input)}
	if err := p.Read(kaitai.NewStream(reader), nil, p); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name string
		want interface{}
	}{
		{"Sum", int(298)}, {"MixedSum", int(4294967293)}, {"MixedLess", true},
		{"NegatedMin", int(128)}, {"DoubledMedium", int(60000)},
		{"ChoiceSmall", int16(-2)}, {"ChoiceMedium", int16(300)},
		{"ComputedBytes", []byte{1, 255}}, {"WideChoice", uint64(9223372036854775813)},
		{"WideSum", uint64(9223372036854775818)}, {"WideBits", uint64(9223372041149743103)},
		{"Shifted", uint64(68719476720)}, {"Modulo", int(298)},
		{"NegativeLessWide", true}, {"WideGreaterNegative", true}, {"NegativeNotEqualWide", true},
		{"WideEqualNegative", false}, {"NegativeGreaterEqualWide", false}, {"WideLessEqualNegative", false},
		{"NegativeLiteralLessWide", true},
		{"WideLiteralChoice", uint64(9223372036854775808)}, {"WideLiteralGreaterNegative", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := reflect.ValueOf(p).MethodByName(tc.name).Call(nil)
			if !got[1].IsNil() {
				t.Fatal(got[1].Interface())
			}
			if !reflect.DeepEqual(got[0].Interface(), tc.want) {
				t.Fatalf("got %v (%T), want %v (%T)", got[0].Interface(), got[0].Interface(), tc.want, tc.want)
			}
		})
	}
	reader.positions = 0
	if got, err := p.ModuloPosition(); err != nil || got != 19 {
		t.Fatalf("modulo position: %v, %v", got, err)
	}
	if reader.positions != 1 {
		t.Fatalf("position operand evaluated %d times", reader.positions)
	}
	if writable, ok := interface{}(p).(interface {
		Check() error
		Write(*kaitai.Writer) error
	}); ok {
		if err := writable.Check(); err != nil {
			t.Fatal(err)
		}
		out := kaitai.NewFixedSeekableBuffer(len(input))
		if err := writable.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), input) {
			t.Fatalf("roundtrip: %x, want %x", out.Bytes(), input)
		}
	}
}
