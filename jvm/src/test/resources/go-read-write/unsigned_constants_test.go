package contract

import (
	"bytes"
	"encoding/binary"
	"math"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func TestUnsignedConstants(t *testing.T) {
	for _, value := range []uint64{0, math.MaxInt64, uint64(math.MaxInt64) + 1, math.MaxUint64} {
		input := make([]byte, 8)
		binary.BigEndian.PutUint64(input, value)
		p := NewUnsignedConstants()
		if err := p.Read(kaitai.NewStream(bytes.NewReader(input)), nil, p); err != nil {
			t.Fatal(err)
		}
		for _, tc := range []struct {
			get  func() (uint64, error)
			want uint64
		}{
			{p.Decimal, math.MaxUint64}, {p.Hexadecimal, math.MaxUint64},
			{p.SignedOverMax, uint64(math.MaxInt64) + 1}, {p.Alias, math.MaxUint64},
			{p.Difference, math.MaxInt64}, {p.EnumInteger, value},
			{p.ChildMax, math.MaxUint64}, {p.GetChild().ParentMax, math.MaxUint64},
			{p.Choice, math.MaxUint64},
			{p.ArrayElement, value}, {p.ArrayFirst, value},
			{p.ArrayLast, 0}, {p.ArrayMax, math.MaxUint64}, {p.ArrayMin, 0},
		} {
			for i := 0; i < 2; i++ { // Exercise both computation and cached return.
				if got, err := tc.get(); err != nil || got != tc.want {
					t.Fatalf("constant: got %d, %v; want %d", got, err, tc.want)
				}
			}
		}
		if got, err := p.SignedMax(); err != nil || got != math.MaxInt64 {
			t.Fatalf("signed boundary: %d, %v", got, err)
		}
		for _, get := range []func() ([]uint64, error){p.Array, p.ArrayAlias, p.ArrayChoice} {
			for i := 0; i < 2; i++ {
				got, err := get()
				if err != nil || len(got) != 3 || got[0] != value || got[1] != math.MaxUint64 || got[2] != 0 {
					t.Fatalf("unsigned array: %v, %v", got, err)
				}
			}
		}
		for _, get := range []func() ([]uint64, error){p.LiteralArray, p.ChildArray} {
			if got, err := get(); err != nil || len(got) != 1 || got[0] != math.MaxUint64 {
				t.Fatalf("literal/nested unsigned array: %v, %v", got, err)
			}
		}
		if got, err := p.Less(); err != nil || !got {
			t.Fatalf("unsigned compared with negative: %v, %v", got, err)
		}
		if !p.GetValue().isDefined() {
			t.Fatalf("known enum value %d rejected", value)
		}
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		output := kaitai.NewFixedSeekableBuffer(8)
		if err := p.Write(kaitai.NewWriter(output)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(output.Bytes(), input) {
			t.Fatalf("roundtrip: %x != %x", output.Bytes(), input)
		}
		p.SetValue(UnsignedConstants_Values__UnsignedMax)
		p.InvalidateEnumInteger()
		if got, err := p.EnumInteger(); err != nil || got != math.MaxUint64 {
			t.Fatalf("mutated enum integer: %d, %v", got, err)
		}
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		output = kaitai.NewFixedSeekableBuffer(8)
		if err := p.Write(kaitai.NewWriter(output)); err != nil {
			t.Fatal(err)
		}
		if binary.BigEndian.Uint64(output.Bytes()) != math.MaxUint64 {
			t.Fatal("mutation narrowed enum")
		}
	}
	if UnsignedConstants_Values(1).isDefined() {
		t.Fatal("unknown enum accepted")
	}
	if UnsignedConstants_Nested_Signed__Minimum != math.MinInt64 || UnsignedConstants_Nested_Signed__Maximum != math.MaxInt64 {
		t.Fatal("signed enum boundaries changed")
	}
}
