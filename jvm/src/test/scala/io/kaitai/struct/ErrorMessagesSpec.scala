package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io._
import java.nio.charset.Charset
import scala.collection.mutable

class ErrorMessagesSpec extends AnyFunSuite with SimpleMatchers {
  // required, because this class is the sole entry point and this test needs
  // version info
  KSVersion.current = Version.version

  val FORMATS_ERR_DIR = "tests/formats_err"
  val CHARSET_UTF8 = Charset.forName("UTF-8")
  val DEFAULT_CONFIG = CLIConfig()

  def getExpected(fn: String): List[String] = {
    val fis = new FileInputStream(fn)
    val isr = new InputStreamReader(fis, CHARSET_UTF8)
    val br = new BufferedReader(isr)

    var hasComment = true
    var expectedLines = mutable.ArrayBuffer[String]()

    var line = br.readLine()
    while (line != null && line.startsWith("#")) {
      expectedLines += line.stripPrefix("#").stripPrefix(" ")
      line = br.readLine()
    }

    expectedLines.toList
  }

  def testOne(f: File): Unit = {
    val fileName = f.getName
    val testName = fileName.stripSuffix(".ksy")
    val fn = FORMATS_ERR_DIR + "/" + fileName
    test(testName) {
      val expected = getExpected(fn)
      val (_, problems) = JavaKSYParser.localFileToSpecs(fn, DEFAULT_CONFIG)
      val problemsStr = problems.map(problem =>
        // replace version-dependent message with a moniker
        problem.message.replace(
          s"but you have ${KSVersion.current}",
          "but you have $KS_VERSION"
        ).replace(FORMATS_ERR_DIR + "/", "")
      )

      problemsStr.mkString("\n") shouldEqualPlainly expected.mkString("\n")
    }
  }

  new File(FORMATS_ERR_DIR).listFiles.
    filter((f) => f.isFile && f.getName.endsWith(".ksy")).
    sorted.foreach((f) => testOne(f))

//  testOne(new File(FORMATS_ERR_DIR + "/attr_invalid_switch_eq.ksy"))
}
