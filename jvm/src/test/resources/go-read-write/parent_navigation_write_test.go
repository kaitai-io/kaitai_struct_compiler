package contract

import (
	"bytes"
	"testing"

	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
)

func TestParentConstruction(t *testing.T) {
	for _, kind := range []byte{0, 1} {
		p := NewParentNavigation()
		p.SetRoot_(p)
		main := NewParentNavigation_Foo()
		main.SetRoot_(p)
		main.SetParent_(p)
		main.SetKind(kind)
		main.SetFlag(41 + kind)
		p.SetMain(main)
		common := NewParentNavigation_Foo_Common()
		common.SetRoot_(p)
		if kind == 0 {
			branch := NewParentNavigation_Foo_Zero()
			branch.SetRoot_(p)
			branch.SetParent_(main)
			branch.SetBranch(common)
			main.SetBuf(branch)
			common.SetParent_(branch)
		} else {
			branch := NewParentNavigation_Foo_One()
			branch.SetRoot_(p)
			branch.SetParent_(main)
			branch.SetBranch(common)
			main.SetBuf(branch)
			common.SetParent_(branch)
		}
		forced := NewParentNavigation_Foo_Common()
		forced.SetRoot_(p)
		forcedParent := NewParentNavigation_Foo_Zero()
		forcedParent.SetRoot_(p)
		forcedParent.SetParent_(main)
		forcedParent.SetBranch(forced)
		forced.SetParent_(forcedParent)
		p.SetForced(forcedParent)
		if got, err := common._writeGetFlag(); err != nil || got != 41+kind {
			t.Fatalf("writer flag: %v %v", got, err)
		}
		common.InvalidateFlag()
		if got, err := common.Flag(); err != nil || got != 41+kind {
			t.Fatalf("constructed flag: %v %v", got, err)
		}
		for _, value := range []interface{ Check() error }{common, forced, forcedParent, main.GetBuf().(interface{ Check() error }), main, p} {
			if err := value.Check(); err != nil {
				t.Fatal(err)
			}
		}
		output := kaitai.NewFixedSeekableBuffer(2)
		if err := p.Write(kaitai.NewWriter(output)); err != nil {
			t.Fatal(err)
		}
		want := []byte{kind, 41 + kind}
		if !bytes.Equal(output.Bytes(), want) {
			t.Fatalf("write %x want %x", output.Bytes(), want)
		}
		read := NewParentNavigation()
		if err := read.Read(kaitai.NewStream(bytes.NewReader(output.Bytes())), nil, read); err != nil {
			t.Fatal(err)
		}
		if got, err := branchOf(read).Flag(); err != nil || got != 41+kind {
			t.Fatalf("reparse flag: %v %v", got, err)
		}
		common.SetParent_(main)
		if err := main.GetBuf().(interface{ Check() error }).Check(); err == nil {
			t.Fatal("wrong parent accepted")
		}
		common.SetParent_(nil)
		if err := main.GetBuf().(interface{ Check() error }).Check(); err == nil {
			t.Fatal("missing parent accepted")
		}
	}
}

func TestWriterParentErrors(t *testing.T) {
	for _, parent := range []kaitai.Struct{nil, &opaqueParent{}, (*ParentNavigation_Foo_Zero)(nil), NewParentNavigation_Foo_Zero()} {
		common := NewParentNavigation_Foo_Common()
		common.SetParent_(parent)
		for attempt := 0; attempt < 2; attempt++ {
			if _, err := common._writeGetFlag(); err == nil {
				t.Fatalf("writer parent %T accepted", parent)
			}
		}
	}
}
