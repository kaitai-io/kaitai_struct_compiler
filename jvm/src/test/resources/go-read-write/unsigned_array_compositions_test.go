package contract

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"
	"reflect"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func TestUnsignedArrayCompositions(t *testing.T) {
	for _, input := range []uint64{0, math.MaxUint64} {
		data := make([]byte, 8)
		binary.BigEndian.PutUint64(data, input)
		p := NewUnsignedArrayCompositions()
		if err := p.Read(kaitai.NewStream(bytes.NewReader(data)), nil, p); err != nil {
			t.Fatal(err)
		}
		choice := input
		bounded := uint32(input)
		if choice == 0 {
			choice = 256
		}
		check := func(value interface{}, err error, expected string) {
			t.Helper()
			if err != nil || fmt.Sprint(value) != expected {
				t.Fatalf("got %v, %v; want %s", value, err, expected)
			}
		}
		for name, want := range map[string]string{
			"Choice": fmt.Sprintf("[%d]", choice), "ChoiceFirst": fmt.Sprint(choice),
			"NamedChoice": fmt.Sprintf("[%d]", choice), "MixedMatrix": fmt.Sprintf("[[%d] [256]]", input),
			"Matrix": fmt.Sprintf("[[%d]]", input), "MatrixValue": fmt.Sprint(input),
			"SignedValues": fmt.Sprintf("[%d -1]", bounded), "SignedPositive": fmt.Sprint(bounded), "SignedNegative": "-1",
			"SignedAlias": fmt.Sprint(bounded),
		} {
			for i := 0; i < 2; i++ {
				result := reflect.ValueOf(p).MethodByName(name).Call(nil)
				var err error
				if !result[1].IsNil() {
					err = result[1].Interface().(error)
				}
				check(result[0].Interface(), err, want)
			}
		}
		v1, err := p._writeGetChoice()
		check(v1, err, fmt.Sprintf("[%d]", choice))
		v2, err := p._writeGetMatrix()
		check(v2, err, fmt.Sprintf("[[%d]]", input))
		v3, err := p._writeGetSignedValues()
		check(v3, err, fmt.Sprintf("[%d -1]", bounded))
		v4, err := p._writeGetChoiceFirst()
		check(v4, err, fmt.Sprint(choice))
		v5, err := p._writeGetMatrixValue()
		check(v5, err, fmt.Sprint(input))
		v6, err := p._writeGetSignedPositive()
		check(v6, err, fmt.Sprint(bounded))
		v7, err := p._writeGetSignedNegative()
		check(v7, err, "-1")
		v8, err := p._writeGetNamedChoice()
		check(v8, err, fmt.Sprintf("[%d]", choice))
		v9, err := p._writeGetMixedMatrix()
		check(v9, err, fmt.Sprintf("[[%d] [256]]", input))
		v10, err := p._writeGetSignedAlias()
		check(v10, err, fmt.Sprint(bounded))
		if err := p.Check(); err != nil {
			t.Fatal(err)
		}
		out := kaitai.NewFixedSeekableBuffer(8)
		if err := p.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(data, out.Bytes()) {
			t.Fatalf("roundtrip: %x != %x", data, out.Bytes())
		}
	}
}

func TestUnsignedArrayMutationRanges(t *testing.T) {
	newValue := func(input uint64) *UnsignedArrayCompositions {
		data := make([]byte, 8)
		binary.BigEndian.PutUint64(data, input)
		p := NewUnsignedArrayCompositions()
		if err := p.Read(kaitai.NewStream(bytes.NewReader(data)), nil, p); err != nil {
			t.Fatal(err)
		}
		return p
	}
	for _, writing := range []bool{false, true} {
		t.Run(fmt.Sprintf("writer=%v", writing), func(t *testing.T) {
			for _, method := range []string{"choice", "matrix", "scalar"} {
				t.Run(method, func(t *testing.T) {
					p := newValue(0)
					values, err := p.SmallValues()
					if err != nil {
						t.Fatal(err)
					}
					values[0] = -1
					switch method {
					case "choice":
						if writing {
							_, err = p._writeGetNamedChoice()
						} else {
							_, err = p.NamedChoice()
						}
					case "matrix":
						if writing {
							_, err = p._writeGetMixedMatrix()
						} else {
							_, err = p.MixedMatrix()
						}
					case "scalar":
						if writing {
							_, err = p._writeGetScalarArray()
						} else {
							_, err = p.ScalarArray()
						}
					}
					want := "Go read-write integer array element cannot be converted from int to uint64 without loss"
					if err == nil || err.Error() != want {
						t.Fatalf("got %v, want %s", err, want)
					}
					if values[0] != -1 {
						t.Fatal("source slice changed")
					}
				})
			}
			p := newValue(0)
			values, err := p.BoundedValues()
			if err != nil {
				t.Fatal(err)
			}
			values[0] = math.MaxUint64
			if writing {
				_, err = p._writeGetSignedFromValues()
			} else {
				_, err = p.SignedFromValues()
			}
			want := "Go read-write integer array element cannot be converted from uint64 to int64 without loss"
			if err == nil || err.Error() != want {
				t.Fatalf("got %v, want %s", err, want)
			}
			values[0] = math.MaxInt64
			var signed []int64
			if writing {
				signed, err = p._writeGetSignedFromValues()
			} else {
				signed, err = p.SignedFromValues()
			}
			if err != nil || len(signed) != 2 || signed[0] != math.MaxInt64 || signed[1] != -1 {
				t.Fatalf("valid boundary after failed conversion: %v, %v", signed, err)
			}

			p = newValue(1)
			small, err := p.SmallValues()
			if err != nil {
				t.Fatal(err)
			}
			small[0] = -1
			var choice []uint64
			if writing {
				choice, err = p._writeGetNamedChoice()
			} else {
				choice, err = p.NamedChoice()
			}
			if err != nil || len(choice) != 1 || choice[0] != 1 {
				t.Fatalf("skipped branch evaluated: %v, %v", choice, err)
			}
		})
	}
}
