package contract

import (
	"bytes"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func TestMixedSignedUnsignedDivision(t *testing.T) {
	for _, tc := range []struct {
		name                                                               string
		signed                                                             int64
		unsigned                                                           uint64
		signedDiv                                                          int64
		unsignedDiv                                                        int64
		signedMod                                                          uint64
		unsignedMod                                                        uint64
		signedDivError, unsignedDivError, signedModError, unsignedModError bool
	}{
		{name: "exact negative", signed: -6, unsigned: 3, signedDiv: -2, unsignedDiv: -1, signedMod: 0, unsignedModError: true},
		{name: "negative remainder", signed: -7, unsigned: 3, signedDiv: -3, unsignedDiv: -1, signedMod: 2, unsignedModError: true},
		{name: "negative divisor", signed: -3, unsigned: 7, signedDiv: -1, unsignedDiv: -3, signedMod: 4, unsignedModError: true},
		{name: "positive operands", signed: 3, unsigned: 7, signedDiv: 0, unsignedDiv: 2, signedMod: 3, unsignedMod: 1},
		{name: "signed minimum exact", signed: -9223372036854775808, unsigned: 1, signedDiv: -9223372036854775808, unsignedDiv: -1, signedMod: 0, unsignedModError: true},
		{name: "signed minimum remainder", signed: -9223372036854775808, unsigned: 3, signedDiv: -3074457345618258603, unsignedDiv: -1, signedMod: 1, unsignedModError: true},
		{name: "boundary magnitudes exact", signed: -9223372036854775808, unsigned: 9223372036854775808, signedDiv: -1, unsignedDiv: -1, signedMod: 0, unsignedModError: true},
		{name: "boundary magnitudes remainder", signed: -9223372036854775808, unsigned: 9223372036854775809, signedDiv: -1, unsignedDiv: -2, signedMod: 1, unsignedModError: true},
		{name: "negative quotient minimum", signed: -1, unsigned: 9223372036854775808, signedDiv: -1, unsignedDiv: -9223372036854775808, signedMod: 9223372036854775807, unsignedModError: true},
		{name: "negative quotient overflow", signed: -1, unsigned: 9223372036854775809, signedDiv: -1, unsignedDivError: true, signedMod: 9223372036854775808, unsignedModError: true},
		{name: "positive quotient overflow", signed: 1, unsigned: 9223372036854775808, signedDiv: 0, unsignedDivError: true, signedMod: 1, unsignedMod: 0},
		{name: "maximum unsigned dividend", signed: -2, unsigned: 18446744073709551615, signedDiv: -1, unsignedDiv: -9223372036854775808, signedMod: 18446744073709551613, unsignedModError: true},
		{name: "positive boundary remainder", signed: 9223372036854775807, unsigned: 18446744073709551615, signedDiv: 0, unsignedDiv: 2, signedMod: 9223372036854775807, unsignedMod: 1},
		{name: "zero signed", signed: 0, unsigned: 5, signedDiv: 0, unsignedDivError: true, signedMod: 0, unsignedModError: true},
		{name: "zero unsigned", signed: -6, unsigned: 0, signedDivError: true, unsignedDiv: 0, signedModError: true, unsignedModError: true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			p := NewNumericMixedDivision(tc.signed, tc.unsigned)
			if err := p.Read(kaitai.NewStream(bytes.NewReader(nil)), nil, p); err != nil {
				t.Fatal(err)
			}
			for _, op := range []struct {
				name      string
				call      func() (int64, error)
				want      int64
				wantError bool
			}{
				{"signed / unsigned", p.SignedDivUnsigned, tc.signedDiv, tc.signedDivError},
				{"unsigned / signed", p.UnsignedDivSigned, tc.unsignedDiv, tc.unsignedDivError},
			} {
				t.Run(op.name, func(t *testing.T) {
					got, err := op.call()
					if (err != nil) != op.wantError || err == nil && got != op.want {
						t.Fatalf("got %v, %v; want %v, error=%v", got, err, op.want, op.wantError)
					}
				})
			}
			for _, op := range []struct {
				name      string
				call      func() (uint64, error)
				want      uint64
				wantError bool
			}{
				{"signed % unsigned", p.SignedModUnsigned, tc.signedMod, tc.signedModError},
				{"unsigned % signed", p.UnsignedModSigned, tc.unsignedMod, tc.unsignedModError},
			} {
				t.Run(op.name, func(t *testing.T) {
					got, err := op.call()
					if (err != nil) != op.wantError || err == nil && got != op.want {
						t.Fatalf("got %v, %v; want %v, error=%v", got, err, op.want, op.wantError)
					}
				})
			}
		})
	}
}

func TestMixedDivisionEvaluatesPositionOnce(t *testing.T) {
	p := NewNumericMixedDivision(-7, 3)
	reader := &positionCounter{Reader: bytes.NewReader(nil)}
	if err := p.Read(kaitai.NewStream(reader), nil, p); err != nil {
		t.Fatal(err)
	}
	reader.positions = 0
	if got, err := p.PositionDivUnsigned(); err != nil || got != -3 {
		t.Fatalf("division: %v, %v", got, err)
	}
	if reader.positions != 1 {
		t.Fatalf("division queried position %d times", reader.positions)
	}
	reader.positions = 0
	if got, err := p.PositionModUnsigned(); err != nil || got != 2 {
		t.Fatalf("modulo: %v, %v", got, err)
	}
	if reader.positions != 1 {
		t.Fatalf("modulo queried position %d times", reader.positions)
	}
	reader.positions = 0
	if got, err := p.UnsignedDivPosition(); err != nil || got != -1 {
		t.Fatalf("division by position: %v, %v", got, err)
	}
	if reader.positions != 1 {
		t.Fatalf("division divisor queried position %d times", reader.positions)
	}
	reader.positions = 0
	if got, err := p.UnsignedModPosition(); err != nil || got != 0 {
		t.Fatalf("modulo position: %v, %v", got, err)
	}
	if reader.positions != 1 {
		t.Fatalf("modulo divisor queried position %d times", reader.positions)
	}
	if got, err := p.LiteralDivUnsigned(); err != nil || got != -2 {
		t.Fatalf("literal division: %v, %v", got, err)
	}
	if got, err := p.LiteralModUnsigned(); err != nil || got != 1 {
		t.Fatalf("literal modulo: %v, %v", got, err)
	}
	if got, err := p.MinimumDivUnsigned(); err != nil || got != -3074457345618258603 {
		t.Fatalf("minimum division: %v, %v", got, err)
	}
	if got, err := p.MinimumModUnsigned(); err != nil || got != 1 {
		t.Fatalf("minimum modulo: %v, %v", got, err)
	}
}
