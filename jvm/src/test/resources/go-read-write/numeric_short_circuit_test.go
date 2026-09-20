package contract

import (
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

// Observe expression evaluation through _io.pos without changing its value.
type shortCircuitReader struct {
	*bytes.Reader
	positions int
	fail      bool
}

func (r *shortCircuitReader) Seek(offset int64, whence int) (int64, error) {
	if whence == io.SeekCurrent && offset == 0 {
		r.positions++
		if r.fail {
			return 0, errors.New("position sentinel")
		}
	}
	return r.Reader.Seek(offset, whence)
}

func TestNumericShortCircuit(t *testing.T) {
	for _, tc := range []struct {
		name         string
		n            uint64
		d            int64
		enabled      bool
		method       func(*NumericShortCircuit) (bool, error)
		want         bool
		positions    int
		wantError    string
		failPosition bool
	}{
		{name: "and division skips zero", method: (*NumericShortCircuit).AndDiv},
		{name: "or division skips zero", method: (*NumericShortCircuit).OrDiv, want: true},
		{name: "and modulo skips zero", method: (*NumericShortCircuit).AndMod},
		{name: "or modulo skips zero", method: (*NumericShortCircuit).OrMod, want: true},
		{name: "and division executes", n: 1, d: 2, method: (*NumericShortCircuit).AndDiv, want: true},
		{name: "or division executes", n: 4, d: 2, method: (*NumericShortCircuit).OrDiv},
		{name: "and modulo executes", n: 4, d: 2, method: (*NumericShortCircuit).AndMod, want: true},
		{name: "or modulo executes", n: 3, d: 2, method: (*NumericShortCircuit).OrMod},
		{name: "and skips position division", method: (*NumericShortCircuit).GuardedDiv, failPosition: true},
		{name: "and skips position modulo", method: (*NumericShortCircuit).GuardedMod, failPosition: true},
		{name: "or skips position division", method: (*NumericShortCircuit).OrPositionDiv, want: true, failPosition: true},
		{name: "or skips position modulo", method: (*NumericShortCircuit).OrPositionMod, want: true, failPosition: true},
		{name: "and division once", n: 1, d: 2, enabled: true, method: (*NumericShortCircuit).GuardedDiv, want: true, positions: 1},
		{name: "and modulo once", n: 4, d: 2, enabled: true, method: (*NumericShortCircuit).GuardedMod, want: true, positions: 1},
		{name: "or division once", n: 1, d: 2, enabled: true, method: (*NumericShortCircuit).OrPositionDiv, want: true, positions: 1},
		{name: "or modulo once", n: 3, d: 2, enabled: true, method: (*NumericShortCircuit).OrPositionMod, positions: 1},
		{name: "nested skips whole branch", method: (*NumericShortCircuit).Nested, failPosition: true},
		{name: "nested inner or skips", enabled: true, method: (*NumericShortCircuit).Nested, want: true, failPosition: true},
		{name: "nested inner and stops", n: 4, d: 2, enabled: true, method: (*NumericShortCircuit).Nested, positions: 1},
		{name: "nested both execute", d: 2, enabled: true, method: (*NumericShortCircuit).Nested, want: true, positions: 2},
		{name: "chain first stops", method: (*NumericShortCircuit).Chained, failPosition: true},
		{name: "chain second stops", enabled: true, method: (*NumericShortCircuit).Chained, failPosition: true},
		{name: "chain third stops", n: 4, d: 2, enabled: true, method: (*NumericShortCircuit).Chained, positions: 1},
		{name: "chain all execute", d: 2, enabled: true, method: (*NumericShortCircuit).Chained, want: true, positions: 2},
		{name: "or chain first stops", method: (*NumericShortCircuit).ChainedOr, want: true, failPosition: true},
		{name: "or chain second stops", enabled: true, method: (*NumericShortCircuit).ChainedOr, want: true, failPosition: true},
		{name: "or chain third stops", n: 1, d: 2, enabled: true, method: (*NumericShortCircuit).ChainedOr, want: true, positions: 1},
		{name: "or chain all execute", n: 3, d: 2, enabled: true, method: (*NumericShortCircuit).ChainedOr, positions: 2},
		{name: "both arithmetic operands once", n: 1, d: 2, enabled: true, method: (*NumericShortCircuit).BothOperands, want: true, positions: 2},
		{name: "both arithmetic operands skipped", method: (*NumericShortCircuit).BothOperands, failPosition: true},
		{name: "division zero propagates", n: 1, enabled: true, method: (*NumericShortCircuit).GuardedDiv, positions: 1, wantError: "division by zero"},
		{name: "modulo zero propagates", n: 1, enabled: true, method: (*NumericShortCircuit).GuardedMod, positions: 1, wantError: "mod divisor <= 0"},
		{name: "modulo negative propagates", n: 1, d: -2, enabled: true, method: (*NumericShortCircuit).OrPositionMod, positions: 1, wantError: "mod divisor <= 0"},
		{name: "division range propagates", n: 9223372036854775808, d: 1, enabled: true, method: (*NumericShortCircuit).GuardedDiv, positions: 1, wantError: "outside int64 range"},
		{name: "or division range propagates", n: 9223372036854775808, d: 1, enabled: true, method: (*NumericShortCircuit).OrPositionDiv, positions: 1, wantError: "outside int64 range"},
		{name: "nested division error propagates", n: 9223372036854775808, d: 1, enabled: true, method: (*NumericShortCircuit).Nested, positions: 1, wantError: "outside int64 range"},
		{name: "chain division error propagates", n: 9223372036854775808, d: 1, enabled: true, method: (*NumericShortCircuit).Chained, positions: 1, wantError: "outside int64 range"},
		{name: "position error propagates", d: 2, enabled: true, method: (*NumericShortCircuit).GuardedDiv, positions: 1, wantError: "position sentinel", failPosition: true},
		{name: "pure boolean true", d: 1, enabled: true, method: (*NumericShortCircuit).PureBoolean, want: true},
		{name: "pure boolean false", n: 1, method: (*NumericShortCircuit).PureBoolean},
	} {
		t.Run(tc.name, func(t *testing.T) {
			p := NewNumericShortCircuit(tc.n, tc.d, tc.enabled)
			reader := &shortCircuitReader{Reader: bytes.NewReader(nil)}
			if err := p.Read(kaitai.NewStream(reader), nil, p); err != nil {
				t.Fatal(err)
			}
			reader.positions = 0
			reader.fail = tc.failPosition
			got, err := tc.method(p)
			if tc.wantError != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantError) {
					t.Fatalf("got %v, %v; want error containing %q", got, err, tc.wantError)
				}
			} else if err != nil || got != tc.want {
				t.Fatalf("got %v, %v; want %v, nil", got, err, tc.want)
			}
			if reader.positions != tc.positions {
				t.Fatalf("position evaluated %d times; want %d", reader.positions, tc.positions)
			}
		})
	}
}
