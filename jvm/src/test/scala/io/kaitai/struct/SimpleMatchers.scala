package io.kaitai.struct

import org.scalactic.Equality
import org.scalactic.source.Position
import org.scalatest._

/**
  * By default, ScalaTest equality matchers for strings try to be nice and do two things:
  *
  * - They add brackets `[]` around differences, trying to highlight what's different.
  * - They abbreviate strings liberally, trying to display only the "interesting" parts. These
  *   heuristics to determine what is "interesting" do not always work as intended, especially with
  *   more complicated, non-text contents.
  *
  * Both these features doesn't really play nice with IDEs, as IDEs (like IDEA) normally have their
  * own way to visualize diffs between strings, and this relies on having access to raw data for
  * both "expected" and "actual" parts.
  *
  * This class provides a `shouldEqualPlainly` matcher, which can be used with ScalaTest to emit
  * plain messages comparing two strings without any extra decoration, thus enabling IDEs diff
  * visualizers to work.
  *
  * Adopted from https://stackoverflow.com/a/60722745/487064
  */
trait SimpleMatchers {
  implicit class PlainEquality[T](leftSideValue: T) {
    // Like should equal, but does not try to mark diffs in strings with square brackets,
    // so that IntelliJ can show a proper diff.
    def shouldEqualPlainly(right: Any)(implicit equality: Equality[T]): Assertion =
      if (!equality.areEqual(leftSideValue, right)) {
        throw new exceptions.TestFailedException(
          (e: exceptions.StackDepthException) => Some(s"""[${leftSideValue}]\n  did not equal\n[${right}]"""),
          None,
          Position.here
        )
      } else Succeeded
  }
}
