package io.kaitai.struct.formats

import io.kaitai.struct.format.{ClassSpec, KSVersion}
import io.kaitai.struct.precompile.LoadImports
import io.kaitai.struct.Version
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import scala.concurrent.Await
import scala.concurrent.duration._

class JavaClassSpecs$Test extends AnyFunSpec {
  KSVersion.current = Version.version

  describe("JavaClassSpecs with LoadImports") {
    it("successfully imports and processes zip.ksy from URL with absolute imports") {
      val firstSpec = ClassSpec.opaquePlaceholder(List("first"))
      val javaClassSpecs = new JavaClassSpecs("", Seq(), firstSpec)

      val url = "https://raw.githubusercontent.com/kaitai-io/kaitai_struct_formats/refs/heads/master/archive/zip.ksy"
      val zipSpec = Await.result(javaClassSpecs.importUrl(url, None), 30.seconds)

      zipSpec shouldBe defined
      zipSpec.get.meta.id should be(Some("zip"))
      zipSpec.get.meta.imports should contain("/common/dos_datetime")

      val loadImports = new LoadImports(javaClassSpecs)
      val workDir = LoadImports.URLImportPath(url).baseDir
      Await.result(loadImports.processClass(zipSpec.get, workDir), 30.seconds)

      javaClassSpecs.get("dos_datetime") shouldBe defined
    }

    it("resolves absolute imports correctly from GitHub URL with refs/heads/branch") {
      val firstSpec = ClassSpec.opaquePlaceholder(List("first"))
      val javaClassSpecs = new JavaClassSpecs("", Seq(), firstSpec)

      val url = "https://raw.githubusercontent.com/kaitai-io/kaitai_struct_formats/refs/heads/master/archive/zip.ksy"
      val zipSpec = Await.result(javaClassSpecs.importUrl(url, None), 30.seconds).get

      val loadImports = new LoadImports(javaClassSpecs)
      val workDir = LoadImports.URLImportPath(url).baseDir
      Await.result(loadImports.processClass(zipSpec, workDir), 30.seconds)

      val dosDatetime = javaClassSpecs.get("dos_datetime")
      dosDatetime shouldBe defined
      dosDatetime.get.meta.id should be(Some("dos_datetime"))
    }

    it("resolves absolute imports correctly from GitHub URL without refs/heads") {
      val firstSpec = ClassSpec.opaquePlaceholder(List("first"))
      val javaClassSpecs = new JavaClassSpecs("", Seq(), firstSpec)

      val url = "https://raw.githubusercontent.com/kaitai-io/kaitai_struct_formats/master/archive/zip.ksy"
      val zipSpec = Await.result(javaClassSpecs.importUrl(url, None), 30.seconds).get

      val loadImports = new LoadImports(javaClassSpecs)
      val workDir = LoadImports.URLImportPath(url).baseDir
      Await.result(loadImports.processClass(zipSpec, workDir), 30.seconds)

      val dosDatetime = javaClassSpecs.get("dos_datetime")
      dosDatetime shouldBe defined
      dosDatetime.get.meta.id should be(Some("dos_datetime"))
    }

    it("imports a spec that has a URL import and resolves it with all dependencies") {
      val specYaml = Map(
        "meta" -> Map(
          "id" -> "test_url_import",
          "imports" -> List(
            "https://raw.githubusercontent.com/kaitai-io/kaitai_struct_formats/refs/heads/master/archive/zip.ksy"
          )
        )
      )
      val testSpec = ClassSpec.fromYaml(specYaml, Some("test.ksy"))

      val javaClassSpecs = new JavaClassSpecs("", Seq(), testSpec)

      val loadImports = new LoadImports(javaClassSpecs)
      Await.result(loadImports.processClass(testSpec, LoadImports.BasePath), 60.seconds)

      val zip = javaClassSpecs.get("zip")
      zip shouldBe defined
      zip.get.meta.id should be(Some("zip"))

      val dosDatetime = javaClassSpecs.get("dos_datetime")
      dosDatetime shouldBe defined
      dosDatetime.get.meta.id should be(Some("dos_datetime"))
    }
  }
}
