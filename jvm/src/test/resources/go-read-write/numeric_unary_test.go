package contract

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func TestNumericUnaryTypes(t *testing.T) {
	for _, tc := range []struct {
		name       string
		unsigned32 uint32
		unsigned64 uint64
		inverted32 int64
		inverted64 uint64
	}{
		{"signed32 boundary", 2147483648, 0, -2147483649, 18446744073709551615},
		{"inverted32 boundary", 2147483647, 9223372036854775808, -2147483648, 9223372036854775807},
	} {
		t.Run(tc.name, func(t *testing.T) {
			p := NewNumericUnary(tc.unsigned32, tc.unsigned64, -128)
			if err := p.Read(kaitai.NewStream(bytes.NewReader(nil)), nil, p); err != nil {
				t.Fatal(err)
			}
			for _, op := range []struct {
				name string
				want interface{}
			}{
				{"LiteralInvertEqual", false}, {"LiteralInvertSigned", int64(-2147483649)},
				{"VariableInvertSigned", tc.inverted32}, {"SmallVariableInvert", int(127)},
				{"SmallLiteralInvert", int(-128)}, {"WideVariableInvert", tc.inverted64},
				{"MinimumSigned", int64(-9223372036854775808)},
				{"MinimumLessUnsigned32", true}, {"Unsigned32GreaterMinimum", true},
				{"MinimumLessUnsigned64", true}, {"Unsigned64GreaterMinimum", true},
				{"MinimumEqualsUnsigned64", false}, {"Unsigned64EqualsMinimum", false},
			} {
				t.Run(op.name, func(t *testing.T) {
					got := reflect.ValueOf(p).MethodByName(op.name).Call(nil)
					if !got[1].IsNil() {
						t.Fatal(got[1].Interface())
					}
					if !reflect.DeepEqual(got[0].Interface(), op.want) {
						t.Fatalf("got %v (%T), want %v (%T)", got[0].Interface(), got[0].Interface(), op.want, op.want)
					}
				})
			}
		})
	}
}
