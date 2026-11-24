package io.kaitai.struct.languages

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PythonCompilerSpec extends AnyFunSpec with Matchers {
  describe("formatImports") {
    it("places standard library imports before other imports") {
      PythonCompiler.formatImports(List(
        "import kaitaistruct",
        "from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO",
        "import collections",
        "from enum import IntEnum",
        "import struct",
        "import zlib",
        "import imported_type",
      )) shouldEqual
        """import collections
          |from enum import IntEnum
          |import struct
          |import zlib
          |
          |import kaitaistruct
          |from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
          |import imported_type
          |""".stripMargin
    }

    it("does not add an empty import group") {
      PythonCompiler.formatImports(List(
        "import kaitaistruct",
        "from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO",
      )) shouldEqual
        """import kaitaistruct
          |from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
          |""".stripMargin
    }
  }
}
