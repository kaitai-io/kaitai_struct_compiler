package contract

import (
	"bytes"
	"errors"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	"io"
	"testing"
)

func TestParameterInstanceReferenceDoesNotActivateWriter(t *testing.T) {
	descriptor := NewWriteInstanceParameter_DescriptorType()
	descriptor.SetValue(0x11)
	descriptor.SetCount(2)
	parent := NewWriteInstanceParameter(descriptor)
	parent.SetPayload([]byte{0xaa, 0xbb})
	parent.SetSuffix(0xcc)
	if err := parent.Check(); err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	if err := parent.Write(kaitai.NewWriter(&out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xaa, 0xbb, 0xcc}) {
		t.Fatalf("parameter output %x", out.Bytes())
	}
	if !descriptor._w_count {
		t.Fatal("lookup consumed another object's pending write")
	}
	if err := descriptor.Check(); err != nil {
		t.Fatal(err)
	}
	own := kaitai.NewSeekableBuffer(nil)
	if err := descriptor.Write(kaitai.NewWriter(own)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(own.Bytes(), []byte{0x11, 0, 0, 0, 0, 2}) {
		t.Fatalf("own write %x", own.Bytes())
	}
	descriptor.SetCount(1)
	parent.SetPayload([]byte{0xdd})
	if err := parent.Check(); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	if err := parent.Write(kaitai.NewWriter(&out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(own.Bytes(), []byte{0x11, 0, 0, 0, 0, 2}) {
		t.Fatalf("lookup wrote stale output %x", own.Bytes())
	}
	if !bytes.Equal(out.Bytes(), []byte{0xdd, 0xcc}) {
		t.Fatalf("mutated parameter %x", out.Bytes())
	}
}

func TestIOValueInstancesKeepReadAndWriteContextsSeparate(t *testing.T) {
	original := []byte{0xaa, 0xbb, 0, 3, 0, 4}
	input := kaitai.NewStream(bytes.NewReader(original))
	v := NewWriteInstanceIoValues()
	if err := v.Read(input, nil, v); err != nil {
		t.Fatal(err)
	}
	readIO, err := v.Output()
	if err != nil || readIO != input {
		t.Fatalf("read IO %v %v", readIO, err)
	}
	readIOs, err := v.Outputs()
	if err != nil || len(readIOs) != 1 || readIOs[0] != input {
		t.Fatalf("read IO array %v %v", readIOs, err)
	}
	if _, err := v.UnusedOutput(); err != nil {
		t.Fatal(err)
	}
	if _, err := v.UnusedOutputs(); err != nil {
		t.Fatal(err)
	}
	v.SetScalar(0x11)
	v.SetArray(0x22)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	first := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(first)); err != nil {
		t.Fatal(err)
	}
	want := []byte{0xaa, 0xbb, 0, 0x11, 0, 0x22}
	if !bytes.Equal(first.Bytes(), want) {
		t.Fatalf("IO aliases %x", first.Bytes())
	}
	second := kaitai.NewSeekableBuffer(nil)
	v.SetScalar(0x33)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	secondWriter := kaitai.NewWriter(second)
	if err := v.Write(secondWriter); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(first.Bytes(), want) {
		t.Fatalf("stale cached output %x", first.Bytes())
	}
	if !bytes.Equal(second.Bytes(), []byte{0xaa, 0xbb, 0, 0x33, 0, 0x22}) {
		t.Fatalf("second IO aliases %x", second.Bytes())
	}
	if p, err := secondWriter.Pos(); err != nil || p != 2 {
		t.Fatalf("IO position %d %v", p, err)
	}
	if readIO, err := v.Output(); err != nil || readIO != input {
		t.Fatalf("writer corrupted scalar reader cache %v %v", readIO, err)
	}
	if readIOs, err := v.Outputs(); err != nil || len(readIOs) != 1 || readIOs[0] != input {
		t.Fatalf("writer corrupted array reader cache %v %v", readIOs, err)
	}
	if !bytes.Equal(original, []byte{0xaa, 0xbb, 0, 3, 0, 4}) {
		t.Fatal("input modified")
	}
}

func TestNestedIOArrayLiteralsAndCastsUseWriterTypes(t *testing.T) {
	input := kaitai.NewStream(bytes.NewReader([]byte{0xaa}))
	v := NewWriteInstanceIoNestedValues()
	if err := v.Read(input, nil, v); err != nil {
		t.Fatal(err)
	}
	matrix, err := v.Matrix()
	if err != nil || len(matrix) != 2 || len(matrix[0]) != 1 || matrix[0][0] != input || matrix[1][0] != input {
		t.Fatalf("read matrix %v %v", matrix, err)
	}
	cube, err := v.Cube()
	if err != nil || len(cube) != 1 || len(cube[0]) != 1 || len(cube[0][0]) != 1 || cube[0][0][0] != input {
		t.Fatalf("read cube %v %v", cube, err)
	}
	cast, err := v.CastOutputs()
	if err != nil || len(cast) != 1 || cast[0] != input {
		t.Fatalf("read cast array %v %v", cast, err)
	}
	scalar, err := v.CastOutput()
	if err != nil || scalar != input {
		t.Fatalf("read cast scalar %v %v", scalar, err)
	}
	v.SetFromMatrix(2)
	v.SetFromCube(3)
	v.SetFromCastOutputs(4)
	v.SetFromCastOutput(5)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	first := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(first)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	want := []byte{0xaa, 0, 2, 0, 3, 0, 4, 0, 5}
	if !bytes.Equal(first.Bytes(), want) {
		t.Fatalf("nested writer values %x", first.Bytes())
	}
	if p, err := w.Pos(); err != nil || p != 1 {
		t.Fatalf("nested writer position %d %v", p, err)
	}
	v.SetFromCube(9)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	second := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(second)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(first.Bytes(), want) {
		t.Fatalf("stale nested writer cache %x", first.Bytes())
	}
	if !bytes.Equal(second.Bytes(), []byte{0xaa, 0, 2, 0, 9, 0, 4, 0, 5}) {
		t.Fatalf("second nested writer values %x", second.Bytes())
	}
	if got, err := v.Matrix(); err != nil || got[0][0] != input || got[1][0] != input {
		t.Fatalf("reader matrix changed %v %v", got, err)
	}
	if got, err := v.Cube(); err != nil || got[0][0][0] != input {
		t.Fatalf("reader cube changed %v %v", got, err)
	}
	if got, err := v.CastOutputs(); err != nil || got[0] != input {
		t.Fatalf("reader cast changed %v %v", got, err)
	}
}

type instanceWriteHook struct {
	*kaitai.SeekableBuffer
	calls  int
	failOn int
	hook   func() error
}

func (w *instanceWriteHook) Write(data []byte) (int, error) {
	w.calls++
	if w.calls == w.failOn {
		return 0, io.ErrClosedPipe
	}
	if w.calls == 2 && w.hook != nil {
		if err := w.hook(); err != nil {
			return 0, err
		}
	}
	return w.SeekableBuffer.Write(data)
}

func newIOValueInstance(t *testing.T) *WriteInstanceIoValues {
	t.Helper()
	v := NewWriteInstanceIoValues()
	v.SetPrefix(0xaa)
	v.SetSuffix(0xbb)
	v.SetScalar(0x11)
	v.SetArray(0x22)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	return v
}

func TestIOValueCachesAndActiveWriterClearOnFailure(t *testing.T) {
	v := newIOValueInstance(t)
	bad := &instanceWriteHook{SeekableBuffer: kaitai.NewSeekableBuffer(nil), failOn: 2}
	if err := v.Write(kaitai.NewWriter(bad)); !errors.Is(err, io.ErrClosedPipe) {
		t.Fatalf("write error %v", err)
	}
	if !bytes.Equal(bad.Bytes(), []byte{0xaa}) {
		t.Fatalf("failure bytes %x", bad.Bytes())
	}
	if v._activeWriter != nil || v._wf_output || v._wf_outputs || v._wv_output != nil || v._wv_outputs != nil {
		t.Fatal("failed write retained active context/cache")
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xaa, 0xbb, 0, 0x11, 0, 0x22}) {
		t.Fatalf("retry alias %x", out.Bytes())
	}
	if v._activeWriter != nil || v._wf_output || v._wf_outputs {
		t.Fatal("successful write retained active context/cache")
	}
}

func TestReentrantWriteRestoresOuterInstanceContext(t *testing.T) {
	for _, failInner := range []bool{false, true} {
		v := newIOValueInstance(t)
		inner := &instanceWriteHook{SeekableBuffer: kaitai.NewSeekableBuffer(nil)}
		if failInner {
			inner.failOn = 2
		}
		innerWriter := kaitai.NewWriter(inner)
		outer := &instanceWriteHook{SeekableBuffer: kaitai.NewSeekableBuffer(nil)}
		outerWriter := kaitai.NewWriter(outer)
		outer.hook = func() error {
			err := v.Write(innerWriter)
			if failInner {
				if !errors.Is(err, io.ErrClosedPipe) {
					t.Fatalf("inner cause %v", err)
				}
			} else if err != nil {
				return err
			}
			if v._activeWriter != outerWriter || v._writer != outerWriter || v._wv_output != outerWriter || v._wv_outputs[0] != outerWriter {
				t.Fatal("inner write corrupted outer context")
			}
			return nil
		}
		if err := v.Write(outerWriter); err != nil {
			t.Fatal(err)
		}
		want := []byte{0xaa, 0xbb, 0, 0x11, 0, 0x22}
		if !bytes.Equal(outer.Bytes(), want) {
			t.Fatalf("outer bytes %x", outer.Bytes())
		}
		if !failInner && !bytes.Equal(inner.Bytes(), want) {
			t.Fatalf("inner bytes %x", inner.Bytes())
		}
		if v._activeWriter != nil {
			t.Fatal("active writer leaked after reentry")
		}
	}
}

func TestChildInstanceReferenceBeforeChildUsesOnlyCachedState(t *testing.T) {
	parent := NewWriteInstanceChildReference()
	parent.SetRoot_(parent)
	parent.SetPayload([]byte{0xaa, 0xbb})
	parent.SetSuffix(0xee)
	child := NewWriteInstanceChildReference_DescriptorType()
	child.SetParent_(parent)
	child.SetRoot_(parent)
	child.SetValue(0x11)
	child.SetCount(2)
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	parent.SetChild(child)
	if err := parent.Check(); err != nil {
		t.Fatal(err)
	}
	first := kaitai.NewSeekableBuffer(nil)
	writer := kaitai.NewWriter(first)
	if err := parent.Write(writer); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(first.Bytes(), []byte{0xaa, 0xbb, 0x11, 0xee, 0, 2}) {
		t.Fatalf("child output %x", first.Bytes())
	}
	if p, err := writer.Pos(); err != nil || p != 4 {
		t.Fatalf("position %d %v", p, err)
	}
	child.SetCount(1)
	parent.SetPayload([]byte{0xdd})
	if err := parent.Check(); err != nil {
		t.Fatal(err)
	}
	failed := kaitai.NewSeekableBuffer(nil)
	var dirty kaitai.ConsistencyNotCheckedError
	if err := parent.Write(kaitai.NewWriter(failed)); !errors.As(err, &dirty) {
		t.Fatalf("unchecked child %v", err)
	}
	if !bytes.Equal(failed.Bytes(), []byte{0xdd}) {
		t.Fatalf("failure suffix %x", failed.Bytes())
	}
	if !bytes.Equal(first.Bytes(), []byte{0xaa, 0xbb, 0x11, 0xee, 0, 2}) {
		t.Fatalf("stale child writer mutated %x", first.Bytes())
	}
	if err := child.Check(); err != nil {
		t.Fatal(err)
	}
	second := kaitai.NewSeekableBuffer(nil)
	if err := parent.Write(kaitai.NewWriter(second)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(second.Bytes(), []byte{0xdd, 0x11, 0xee, 0, 0, 1}) {
		t.Fatalf("retry output %x", second.Bytes())
	}
}

func TestFailedInstanceWriteDoesNotLeaveActiveWriter(t *testing.T) {
	descriptor := NewWriteInstanceParameter_DescriptorType()
	descriptor.SetValue(7)
	descriptor.SetCount(2)
	if err := descriptor.Check(); err != nil {
		t.Fatal(err)
	}
	bad := &instanceSeekFailure{kaitai.NewSeekableBuffer(nil), true}
	if err := descriptor.Write(kaitai.NewWriter(bad)); !errors.Is(err, io.ErrClosedPipe) {
		t.Fatalf("seek error %v", err)
	}
	parent := NewWriteInstanceParameter(descriptor)
	parent.SetPayload([]byte{0xaa, 0xbb})
	parent.SetSuffix(0xcc)
	if err := parent.Check(); err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	if err := parent.Write(kaitai.NewWriter(&out)); err != nil {
		t.Fatalf("stale failed writer reused: %v", err)
	}
	if !bytes.Equal(bad.Bytes(), []byte{7}) {
		t.Fatalf("failed output mutated %x", bad.Bytes())
	}
	if !bytes.Equal(out.Bytes(), []byte{0xaa, 0xbb, 0xcc}) {
		t.Fatalf("parent after failure %x", out.Bytes())
	}
}

func TestPositionedInstancesSetWithoutRead(t *testing.T) {
	v := NewWriteInstances()
	v.SetPrefix(0xa1)
	v.SetMarker(0x1234)
	v.SetArray([]uint8{0xb2, 0xc3})
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(out)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xa1, 0, 0, 0x34, 0x12, 0, 0xb2, 0xc3}) {
		t.Fatalf("bytes %x", out.Bytes())
	}
	if p, err := w.Pos(); err != nil || p != 1 {
		t.Fatalf("position %d: %v", p, err)
	}
	if v._f_lazy {
		t.Fatal("write materialized lazy instance")
	}
	if v._w_marker || v._w_array {
		t.Fatal("successful write left pending flags")
	}
}

func TestPositionedInstanceLifecycle(t *testing.T) {
	v := NewWriteInstances()
	if err := v.Read(kaitai.NewStream(bytes.NewReader([]byte{1, 0, 0, 2, 3, 0, 4, 5})), nil, v); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{1}) {
		t.Fatalf("unrelated write %x", out.Bytes())
	}
	if got, err := v.Marker(); err != nil || got != 0x0302 {
		t.Fatalf("lazy value %x %v", got, err)
	}
	v.SetMarker(0x6655)
	var dirty kaitai.ConsistencyNotCheckedError
	if err := v.Write(kaitai.NewWriter(out)); !errors.As(err, &dirty) {
		t.Fatalf("setter dirty: %v", err)
	}
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 2; i++ {
		out = kaitai.NewSeekableBuffer(nil)
		if err := v.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), []byte{1, 0, 0, 0x55, 0x66}) {
			t.Fatalf("repeat %x", out.Bytes())
		}
	}
	v.SetMarkerEnabled_(false)
	if err := v.Write(kaitai.NewWriter(out)); !errors.As(err, &dirty) {
		t.Fatalf("disable dirty: %v", err)
	}
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{1}) {
		t.Fatalf("disabled %x", out.Bytes())
	}
	v.SetMarkerEnabled_(true)
	v.InvalidateMarker_()
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{1}) || v._f_marker {
		t.Fatalf("invalidated %x", out.Bytes())
	}
	v.SetArray([]uint8{1})
	var inconsistent kaitai.ConsistencyError
	if err := v.Check(); !errors.As(err, &inconsistent) {
		t.Fatalf("array check: %v", err)
	}
	v.SetArrayEnabled_(false)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
}

type instanceSeekFailure struct {
	*kaitai.SeekableBuffer
	fail bool
}

func (s *instanceSeekFailure) Seek(n int64, whence int) (int64, error) {
	if s.fail && whence == io.SeekStart {
		return 0, io.ErrClosedPipe
	}
	return s.SeekableBuffer.Seek(n, whence)
}

type instanceRestoreFailure struct{ *kaitai.SeekableBuffer }

func (s *instanceRestoreFailure) Seek(n int64, whence int) (int64, error) {
	if whence == io.SeekStart && n == 1 {
		return 0, io.ErrClosedPipe
	}
	return s.SeekableBuffer.Seek(n, whence)
}

func TestPositionedRestoreErrorStopsLaterInstances(t *testing.T) {
	v := NewWriteInstances()
	v.SetPrefix(7)
	v.SetArray([]uint8{8, 9})
	v.SetMarker(0x1234)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := &instanceRestoreFailure{kaitai.NewSeekableBuffer(nil)}
	if err := v.Write(kaitai.NewWriter(out)); !errors.Is(err, io.ErrClosedPipe) {
		t.Fatalf("restore cause %v", err)
	}
	// Instances are emitted in declaration-name order: array fails restoring,
	// so marker must never write at offsets 3 and 4.
	if !bytes.Equal(out.Bytes(), []byte{7, 0, 0, 0, 0, 0, 8, 9}) {
		t.Fatalf("restore bytes %x", out.Bytes())
	}
	if !v._w_array {
		t.Fatal("failed instance no longer pending")
	}
}

func TestPositionedReadInitializesZeroValueAndResetsCaches(t *testing.T) {
	var v WriteInstances
	first := []byte{1, 0, 0, 2, 3, 0, 4, 5}
	if err := v.Read(kaitai.NewStream(bytes.NewReader(first)), nil, &v); err != nil {
		t.Fatal(err)
	}
	if _, err := v.Marker(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), first[:5]) {
		t.Fatalf("zero-value read %x", out.Bytes())
	}
	if _, err := v.Array(); err != nil {
		t.Fatal(err)
	}
	if err := v.Read(kaitai.NewStream(bytes.NewReader([]byte{9, 0, 0, 6, 7, 0, 8, 9})), nil, &v); err != nil {
		t.Fatal(err)
	}
	if got, err := v.Marker(); err != nil || got != 0x0706 {
		t.Fatalf("stale cache %x %v", got, err)
	}
	if got, err := v.Array(); err != nil || !bytes.Equal(got, []byte{8, 9}) {
		t.Fatalf("stale array %x %v", got, err)
	}
}

func TestPositionedMissingCachedDependencyFailsWithoutRead(t *testing.T) {
	v := NewWriteInstanceTiming()
	v.SetPrefix(1)
	v.SetPayload(nil)
	v.SetSuffix(9)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	var inconsistent kaitai.ConsistencyError
	if err := v.Write(kaitai.NewWriter(out)); !errors.As(err, &inconsistent) {
		t.Fatalf("missing cached dependency %v", err)
	}
	if !bytes.Equal(out.Bytes(), []byte{1}) {
		t.Fatalf("suffix emitted %x", out.Bytes())
	}
	if v._f_marker {
		t.Fatal("missing dependency attempted lazy read")
	}
}

func TestFailedLazyReadIsNotMaterialized(t *testing.T) {
	v := NewWriteInstances()
	if err := v.Read(kaitai.NewStream(bytes.NewReader([]byte{1})), nil, v); err != nil {
		t.Fatal(err)
	}
	if _, err := v.Marker(); !errors.Is(err, io.EOF) {
		t.Fatalf("lazy EOF %v", err)
	}
	if v._f_marker {
		t.Fatal("failed read became serializable")
	}
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{1}) {
		t.Fatalf("failed read serialized %x", out.Bytes())
	}
}

func TestUnpositionedInstanceWritesAtReference(t *testing.T) {
	v := NewWriteInstanceUnpositioned()
	v.SetPrefix(0xaa)
	v.SetCount(1)
	v.SetPayload([]byte{0xbb})
	v.SetSuffix(0xcc)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	if err := v.Write(kaitai.NewWriter(&out)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xaa, 1, 0xbb, 0xcc}) {
		t.Fatalf("unpositioned %x", out.Bytes())
	}
}

func TestPositionedInstanceUsesOutputParameter(t *testing.T) {
	v := NewWriteInstanceIo(nil)
	v.SetPrefix(1)
	v.SetData(9)
	primary := kaitai.NewSeekableBuffer(nil)
	secondary := kaitai.NewSeekableBuffer(nil)
	secondaryWriter := kaitai.NewWriter(secondary)
	v.SetInputWriter_(secondaryWriter)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	if err := v.Write(kaitai.NewWriter(primary)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(primary.Bytes(), []byte{1}) || !bytes.Equal(secondary.Bytes(), []byte{0, 0, 9}) {
		t.Fatalf("IO params %x %x", primary.Bytes(), secondary.Bytes())
	}
	if p, err := secondaryWriter.Pos(); err != nil || p != 0 {
		t.Fatalf("secondary position %d %v", p, err)
	}
}

func TestPositionedInstanceSeekFailures(t *testing.T) {
	v := NewWriteInstances()
	v.SetPrefix(7)
	v.SetMarker(9)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	var plain bytes.Buffer
	if err := v.Write(kaitai.NewWriter(&plain)); !errors.Is(err, kaitai.ErrWriterNotSeekable) {
		t.Fatalf("nonseeker %v", err)
	}
	if !bytes.Equal(plain.Bytes(), []byte{7}) {
		t.Fatalf("nonseeker bytes %x", plain.Bytes())
	}
	bad := &instanceSeekFailure{kaitai.NewSeekableBuffer(nil), true}
	if err := v.Write(kaitai.NewWriter(bad)); !errors.Is(err, io.ErrClosedPipe) {
		t.Fatalf("seek cause %v", err)
	}
	if !bytes.Equal(bad.Bytes(), []byte{7}) {
		t.Fatalf("failed seek bytes %x", bad.Bytes())
	}
	bad.fail = false
	if _, err := bad.Seek(0, io.SeekStart); err != nil {
		t.Fatal(err)
	}
	if err := v.Write(kaitai.NewWriter(bad)); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(bad.Bytes(), []byte{7, 0, 0, 9, 0}) {
		t.Fatalf("retry %x", bad.Bytes())
	}
}

func TestPositionedInstanceTimingThroughValue(t *testing.T) {
	v := NewWriteInstanceTiming()
	v.SetPrefix(0x11)
	v.SetPayload([]byte{0xaa, 0xbb})
	v.SetSuffix(0xcc)
	v.SetMarker(2)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	if v._f_count {
		t.Fatal("Check evaluated IO-dependent value instance")
	}
	out := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(out)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0x11, 0xaa, 0xbb, 0xcc, 0, 2}) {
		t.Fatalf("timing %x", out.Bytes())
	}
	if p, err := w.Pos(); err != nil || p != 4 {
		t.Fatalf("position %d %v", p, err)
	}
	v.SetMarker(3)
	v.SetPayload([]byte{0xaa, 0xbb, 0xdd})
	v.InvalidateCount()
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out = kaitai.NewSeekableBuffer(nil)
	w = kaitai.NewWriter(out)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0x11, 0xaa, 0xbb, 0xdd, 0xcc, 3}) {
		t.Fatalf("mutated timing %x", out.Bytes())
	}
}

func newPositionedNested(t *testing.T, offset uint8) *WriteInstanceNested {
	t.Helper()
	v := NewWriteInstanceNested()
	v.SetRoot_(v)
	v.SetPrefix(0xa1)
	v.SetSuffix(0xee)
	c := NewWriteInstanceNested_Nested()
	c.SetParent_(v)
	c.SetRoot_(v)
	c.SetPrefix(0xb2)
	c.SetOffset(offset)
	c.SetPayload(0x1234)
	if err := c.Check(); err != nil {
		t.Fatal(err)
	}
	v.SetChild(c)
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	return v
}

func TestPositionedNestedDescendantsBeforeProcessing(t *testing.T) {
	v := newPositionedNested(t, 2)
	out := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(out)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xa1, 0, 0, 0, 0xa3, 0x13, 0x25, 0x03, 0, 0, 0xee}) {
		t.Fatalf("nested %x", out.Bytes())
	}
	if p, err := w.Pos(); err != nil || p != 1 {
		t.Fatalf("position %d %v", p, err)
	}
	if v._w_child || v.child._w_payload {
		t.Fatal("pending nested flags")
	}
}

func TestPositionedRepeatedNestedWriteAndInvalidateRead(t *testing.T) {
	v := NewWriteInstanceNestedRepeat()
	v.SetRoot_(v)
	v.SetPrefix(0xaa)
	first := NewWriteInstanceNestedRepeat_Nested()
	first.SetParent_(v)
	first.SetRoot_(v)
	first.SetPrefix(0x21)
	first.SetTail(0x43)
	second := NewWriteInstanceNestedRepeat_Nested()
	second.SetParent_(v)
	second.SetRoot_(v)
	second.SetPrefix(0x65)
	second.SetTail(0x87)
	for _, child := range []*WriteInstanceNestedRepeat_Nested{first, second} {
		if err := child.Check(); err != nil {
			t.Fatal(err)
		}
	}
	v.SetChildren([]*WriteInstanceNestedRepeat_Nested{first, second})
	if err := v.Check(); err != nil {
		t.Fatal(err)
	}
	out := kaitai.NewSeekableBuffer(nil)
	w := kaitai.NewWriter(out)
	if err := v.Write(w); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(out.Bytes(), []byte{0xaa, 0, 0x30, 0x52, 0x74, 0x96}) {
		t.Fatalf("repeated children %x", out.Bytes())
	}
	if p, err := w.Pos(); err != nil || p != 1 {
		t.Fatalf("repeat pos %d %v", p, err)
	}
	parsed := NewWriteInstanceNestedRepeat()
	if err := parsed.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())), nil, parsed); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 2; i++ {
		children, err := parsed.Children()
		if err != nil || len(children) != 2 {
			t.Fatalf("children read %v %v", children, err)
		}
		if tail, err := children[1].Tail(); err != nil || tail != 0x87 {
			t.Fatalf("child tail %x %v", tail, err)
		}
		if len(parsed._raw_children) != 2 || len(parsed._raw__raw_children) != 2 {
			t.Fatal("stale raw arrays")
		}
		parsed.InvalidateChildren_()
	}
}

func TestPositionedNestedFailureSuppressesLaterInstance(t *testing.T) {
	v := newPositionedNested(t, 3)
	out := kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); !errors.Is(err, kaitai.ErrBufferCapacity) {
		t.Fatalf("overflow %v", err)
	}
	if len(out.Bytes()) > 8 {
		t.Fatalf("later instance written %x", out.Bytes())
	}
	v.child.SetOffset(2)
	var dirty kaitai.ConsistencyNotCheckedError
	if err := v.Write(kaitai.NewWriter(kaitai.NewSeekableBuffer(nil))); !errors.As(err, &dirty) {
		t.Fatalf("child unchecked %v", err)
	}
	if err := v.child.Check(); err != nil {
		t.Fatal(err)
	}
	out = kaitai.NewSeekableBuffer(nil)
	if err := v.Write(kaitai.NewWriter(out)); err != nil {
		t.Fatal(err)
	}
	if len(out.Bytes()) != 11 || out.Bytes()[10] != 0xee {
		t.Fatalf("retry %x", out.Bytes())
	}
}

func TestPositionedSwitchedEndian(t *testing.T) {
	for _, tc := range []struct {
		kind uint8
		data uint32
		want []byte
	}{{1, 0x1234, []byte{1, 0, 0x34, 0x12}}, {2, 0x12345678, []byte{2, 0, 0x12, 0x34, 0x56, 0x78}}} {
		v := NewWriteInstanceSwitch(tc.kind)
		v.SetPrefix(tc.kind)
		v.SetData(tc.data)
		if err := v.Check(); err != nil {
			t.Fatal(err)
		}
		out := kaitai.NewSeekableBuffer(nil)
		w := kaitai.NewWriter(out)
		if err := v.Write(w); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(out.Bytes(), tc.want) {
			t.Fatalf("switch %x", out.Bytes())
		}
		if p, err := w.Pos(); err != nil || p != 1 {
			t.Fatalf("switch pos %d %v", p, err)
		}
		v.SetDataArray([]uint32{0x1234, 0x5678})
		if err := v.Check(); err != nil {
			t.Fatal(err)
		}
		out = kaitai.NewSeekableBuffer(nil)
		if err := v.Write(kaitai.NewWriter(out)); err != nil {
			t.Fatal(err)
		}
		want := []byte{0x34, 0x12, 0x78, 0x56}
		if tc.kind == 2 {
			want = []byte{0, 0, 0x12, 0x34, 0, 0, 0x56, 0x78}
		}
		if !bytes.Equal(out.Bytes()[8:], want) {
			t.Fatalf("switch array %x", out.Bytes())
		}
		parsed := NewWriteInstanceSwitch(tc.kind)
		if err := parsed.Read(kaitai.NewStream(bytes.NewReader(out.Bytes())), nil, parsed); err != nil {
			t.Fatal(err)
		}
		got, err := parsed.DataArray()
		if err != nil || len(got) != 2 || got[0] != 0x1234 || got[1] != 0x5678 {
			t.Fatalf("switch array read %v %v", got, err)
		}
	}
}
