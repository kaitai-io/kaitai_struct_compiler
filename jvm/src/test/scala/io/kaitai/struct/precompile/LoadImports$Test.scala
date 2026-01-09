package io.kaitai.struct.precompile

import io.kaitai.struct.precompile.LoadImports._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class LoadImports$Test extends AnyFunSpec {
  describe("LoadImports.ImportPath") {
    describe("fromString") {
      it("recognizes http:// URLs") {
        val result = ImportPath.fromString("http://example.com/file.ksy")
        result shouldBe a[URLImportPath]
        result.asInstanceOf[URLImportPath].url should be("http://example.com/file.ksy")
      }

      it("recognizes https:// URLs") {
        val result = ImportPath.fromString("https://example.com/file.ksy")
        result shouldBe a[URLImportPath]
        result.asInstanceOf[URLImportPath].url should be("https://example.com/file.ksy")
      }

      it("treats relative paths as RelativeImportPath") {
        val result = ImportPath.fromString("relative/path.ksy")
        result shouldBe a[RelativeImportPath]
      }

      it("treats absolute paths as AbsoluteImportPath") {
        val result = ImportPath.fromString("/absolute/path.ksy")
        result shouldBe a[AbsoluteImportPath]
      }
    }

    describe("URLImportPath.baseDir") {
      it("extracts base directory from URL with multiple path segments") {
        val urlPath = URLImportPath("https://example.com/dir/subdir/file.ksy")
        urlPath.baseDir should be(URLImportPath("https://example.com/dir/subdir"))
      }

      it("extracts base directory from URL with single path segment") {
        val urlPath = URLImportPath("https://example.com/file.ksy")
        urlPath.baseDir should be(URLImportPath("https://example.com"))
      }

      it("returns itself for URL without path") {
        val urlPath = URLImportPath("https://example.com")
        urlPath.baseDir should be(urlPath)
      }

      it("does not cut into protocol portion") {
        val urlPath = URLImportPath("https://example.com/file.ksy")
        val base = urlPath.baseDir
        base should be(URLImportPath("https://example.com"))
        base.asInstanceOf[URLImportPath].url should startWith("https://")
      }
    }

    describe("ImportPath.add") {
      it("treats URLs as absolute paths") {
        val relWorkDir = RelativeImportPath(List("local", "dir"))
        val absoluteUrl = URLImportPath("https://example.com/file.ksy")
        val result = ImportPath.add(relWorkDir, absoluteUrl)
        result should be(absoluteUrl)
      }

      it("combines URL base with relative path") {
        val urlBase = URLImportPath("https://example.com/dir")
        val relPath = RelativeImportPath(List("subdir", "file.ksy"))
        val result = ImportPath.add(urlBase, relPath)
        result should be(URLImportPath("https://example.com/dir/subdir/file.ksy"))
      }

      it("preserves absolute paths") {
        val relWorkDir = RelativeImportPath(List("local"))
        val absPath = AbsoluteImportPath(List("absolute", "path.ksy"))
        val result = ImportPath.add(relWorkDir, absPath)
        result should be(absPath)
      }
    }
  }
}
