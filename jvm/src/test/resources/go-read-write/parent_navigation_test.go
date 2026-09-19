package contract

import (
	"bytes"
	"strings"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

// Handwritten and opaque structs must keep satisfying the original runtime API.
type opaqueParent struct{}

func (*opaqueParent) IO_() *kaitai.Stream { return nil }

var _ kaitai.Struct = (*opaqueParent)(nil)

func branchOf(p *ParentNavigation) *ParentNavigation_Foo_Common {
	switch b := p.Main.Buf.(type) {
	case *ParentNavigation_Foo_Zero:
		return b.Branch
	case *ParentNavigation_Foo_One:
		return b.Branch
	default:
		panic("unexpected branch")
	}
}

func TestParentNavigation(t *testing.T) {
	for _, kind := range []byte{0, 1} {
		p := NewParentNavigation()
		external := NewParentNavigation_Foo()
		if err := external.Read(kaitai.NewStream(bytes.NewReader([]byte{0, 93})), nil, p); err != nil {
			t.Fatal(err)
		}
		if err := p.Read(kaitai.NewStream(bytes.NewReader([]byte{kind, 31 + kind})), external, p); err != nil {
			t.Fatal(err)
		}
		branch := branchOf(p)
		if got, err := p.Parent(); err != nil || got != 7 {
			t.Fatalf("schema parent instance collided: %v %v", got, err)
		}
		bridge := NewParentNavigationExternal()
		if err := bridge.Read(kaitai.NewStream(bytes.NewReader(nil)), external, nil); err != nil {
			t.Fatal(err)
		}
		foreignChild := NewParentNavigation_Foo_Common()
		if err := foreignChild.Read(kaitai.NewStream(bytes.NewReader(nil)), bridge, p); err != nil {
			t.Fatal(err)
		}
		if got, err := foreignChild.Flag(); err != nil || got != 93 {
			t.Fatalf("separately generated parent: %v %v", got, err)
		}
		if bridge.Parent_() != external {
			t.Fatal("separately generated parent identity lost")
		}
		if branch._parent != p.Main.Buf || p.Forced._parent != p.Main || p.Forced.Branch._parent != p.Forced {
			t.Fatal("concrete branch identity lost")
		}
		for _, common := range []*ParentNavigation_Foo_Common{branch, p.Forced.Branch} {
			for repeat := 0; repeat < 2; repeat++ {
				if got, err := common.Flag(); err != nil || got != 31+kind {
					t.Fatalf("flag: %v %v", got, err)
				}
				if got, err := common.ExternalFlag(); err != nil || got != 93 {
					t.Fatalf("external parent: %v %v", got, err)
				}
			}
		}
	}
}

func TestParentNavigationErrors(t *testing.T) {
	var nilRoot *ParentNavigation
	if nilRoot.Parent_() != nil || NewParentNavigation().Parent_() != nil {
		t.Fatal("nil/root accessor must return nil")
	}
	for _, parent := range []kaitai.Struct{nil, &opaqueParent{}, (*opaqueParent)(nil), (*ParentNavigation_Foo_Zero)(nil), NewParentNavigation_Foo_Zero()} {
		p := NewParentNavigation_Foo_Common()
		if err := p.Read(kaitai.NewStream(bytes.NewReader(nil)), parent, nil); err != nil {
			t.Fatal(err)
		}
		if _, err := p.Flag(); err == nil || !strings.Contains(err.Error(), "parent") {
			t.Fatalf("parent %T: expected contextual error, got %v", parent, err)
		}
	}
	// A typed nil can also be hidden in a generated root's generic parent field.
	root := NewParentNavigation()
	if err := root.Read(kaitai.NewStream(bytes.NewReader([]byte{0, 1})), (*ParentNavigation_Foo)(nil), root); err != nil {
		t.Fatal(err)
	}
	common := NewParentNavigation_Foo_Common()
	if err := common.Read(kaitai.NewStream(bytes.NewReader(nil)), root, root); err != nil {
		t.Fatal(err)
	}
	if _, err := common.Flag(); err == nil {
		t.Fatal("typed nil parent returned without an error")
	}
}
