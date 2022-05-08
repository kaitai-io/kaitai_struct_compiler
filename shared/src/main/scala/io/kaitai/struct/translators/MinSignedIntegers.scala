package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

/**
  * Special handling of the minimum values of 32-bit (`-2**31 = -0x8000_0000`) and 64-bit (`-2**63 =
  * -0x8000_0000_0000_0000`) signed integer types. Almost all languages have a unary minus (`-`) as
  * a normal unary operator; it takes its operand (usually a positive integer) and negates it.
  * However, there is no positive counterpart to
  *
  * 1. `-2**31` in signed 32-bit integers (the maximum `sint32` is `2**31 - 1`);
  * 2. `-2**64` in signed 64-bit integers (the maximum `sint64` is `2**63 - 1`).
  *
  * `2**31` and `2**63` generally require either an *un*signed {32,64}-bit integer, or a signed
  * integer of a larger size (e.g. `sint64` for `2**31`). But these types may not be available in
  * all languages/platforms and it doesn't make sense to rely on them just to be able to get the
  * minimum negative value.
  */
trait MinSignedIntegers
  extends AbstractTranslator
  with CommonLiterals {
  override def doIntLiteral(n: BigInt): String = {
    if (n == Long.MinValue || n == Int.MinValue) {
      translate(
        Ast.expr.BinOp(
          Ast.expr.IntNum(n + 1),
          Ast.operator.Sub,
          Ast.expr.IntNum(1)
        )
      )
    } else {
      super.doIntLiteral(n)
    }
  }
}
