package io.kaitai.struct.languages.components
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

trait GenericChecks extends LanguageCompiler with EveryReadIsExpression {
  override def attrCheck(attr: AttrLikeSpec, id: Identifier): Unit = {
    val bodyShouldDependOnIo: Option[Boolean] =
      if (userExprDependsOnIo(attr.cond.ifExpr)) {
        return
      } else {
        Some(false)
      }

    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, attr.cond.repeat, bodyShouldDependOnIo)
        attrValidCheck(attr, bodyShouldDependOnIo)
        condRepeatCommonFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        attrRepeatExprCheck(id, repeatExpr, bodyShouldDependOnIo)
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, attr.cond.repeat, bodyShouldDependOnIo)
        attrValidCheck(attr, bodyShouldDependOnIo)
        condRepeatCommonFooter
      case repUntil: RepeatUntil =>
        attrAssertUntilNotEmpty(id)
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, attr.cond.repeat, bodyShouldDependOnIo)
        attrValidCheck(attr, bodyShouldDependOnIo)
        attrAssertUntilCond(id, attr.dataType, repUntil, bodyShouldDependOnIo)
        condRepeatCommonFooter
      case NoRepeat =>
        attrCheck2(id, attr.dataType, attr.cond.repeat, bodyShouldDependOnIo)
        attrValidCheck(attr, bodyShouldDependOnIo)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }



  def userExprDependsOnIo(expr: Option[Ast.expr]): Boolean = expr match {
    case None => false
    case Some(v) => userExprDependsOnIo(v)
  }

  def getArrayItemType(dt: DataType): DataType = {
    dt match {
      case arr: ArrayType => getArrayItemType(arr.elType)
      case other => other
    }
  }

  def userExprDependsOnIo(expr: Ast.expr): Boolean = {
    expr match {
      case _: Ast.expr.IntNum => false
      case _: Ast.expr.FloatNum => false
      case _: Ast.expr.Str => false
      case Ast.expr.InterpolatedStr(values: Seq[Ast.expr]) =>
        values.exists(v => userExprDependsOnIo(v))
      case _: Ast.expr.Bool => false
      case Ast.expr.EnumById(_, id, _) =>
        userExprDependsOnIo(id)
      case _: Ast.expr.EnumByLabel => false
      case n: Ast.expr.Name =>
        val t = getArrayItemType(translator.detectType(n))
        if (t == KaitaiStreamType || t == OwnedKaitaiStreamType) {
          true
        } else {
          /** @see [[ClassTypeProvider.determineType(inClass:ClassSpec,attrName:String):DataType*]] */
          n.id.name match {
            case Identifier.ROOT
              | Identifier.PARENT
              | Identifier.IO
              | Identifier.ITERATOR
              | Identifier.SWITCH_ON
              | Identifier.INDEX
              | Identifier.SIZEOF
              => false
            case _ =>
              val spec = typeProvider.resolveMember(typeProvider.nowClass, n.id.name)
              spec match {
                case _: AttrSpec => false

                // Parameters are fine because they are normally set by the user, so are already
                // available in _check(). The only parameters set by the generated code in _write()
                // (not by the user) are params of type KaitaiStream or an array of KaitaiStream,
                // but these were caught earlier in this function.
                case _: ParamDefSpec => false

                // Value instances are OK to use in _check() if their expressions in `value` and
                // `if` do not use _io or parse instances. They can refer to other value instances,
                // provided they follow the same conditions (which is ensured by a recursive call).
                case vis: ValueInstanceSpec =>
                  userExprDependsOnIo(vis.ifExpr) || userExprDependsOnIo(vis.value)

                // Although accessing parse instances in _check() is not a problem by itself,
                // because parse instances are set by the user so they are already available in
                // _check(), it becomes a problem when you don't invoke a parse instance dependent
                // on the time of invocation in _write() because you have already done a particular
                // check in _check().
                //
                // Take the test
                // https://github.com/kaitai-io/kaitai_struct_tests/blob/010efd1d9c07a61a320a644d4e782dd488ba28e4/formats/instance_in_repeat_until.ksy
                // as an example. In _write() you don't need to reproduce the special `do { ... }
                // while (!repeat-until);` loop as used in _read(), because you already know the
                // array length, so a simple "foreach" loop will suffice. Then there is a
                // consistency check to ensure that the `repeat-until` condition is `false` for all
                // items except the last one, and `true` for the last one. This check can be either
                // done in _check(), or in _write() at the end of each iteration of the "foreach"
                // loop. You can do it in _check() if you want, but you *need* to evaluate the
                // `repeat-until` expression (and throw away the result, if you like - the point is
                // just to invoke the parse instances specified there) at the end of each "foreach"
                // loop iteration in _write(), because _read() does that. So it makes sense to do
                // the check only in _write().
                //
                // It may be tempting to suggest to do the check both in _check() and _write(), and
                // in this particular case you could really do that because the parse instance is
                // used directly in the `repeat-until` expression. But if such parse instance is used
                // indirectly via a value instance, you should no longer use that value instance in
                // _check() at all, because that would cache the its value and the invocation in
                // _write() would merely return this cached value, not evaluating the expression
                // again. But that means that the parse instance will be written at a different
                // time, because it won't be invoked from `seq` at the time it would be in _read()
                // and will be written only when invoked from _fetchInstances(), which is wrong and
                // inconsistent with parsing. Although the user could work around this specific
                // issue by manually invalidating the value instances that the careless _check()
                // invoked after calling it, this would be a bug in _check(). Calling _check()
                // should not have side effects that the user has to "undo".
                //
                // Of course, perhaps most parse instances are not dependent on the time of
                // invocation. But the language allows them to be, and it's not that trivial to
                // detect it: you have to analyze all expressions that affect its parsing. So we
                // will not do that for now - it's easier to avoid invoking parse instances in
                // _check() (directly or indirectly) entirely.
                case _: ParseInstanceSpec => true
              }
          }
        }
      case Ast.expr.InternalName(id) =>
        ???
      case Ast.expr.UnaryOp(op, inner) =>
        userExprDependsOnIo(inner)
      case Ast.expr.Compare(left, op, right) =>
        userExprDependsOnIo(left) || userExprDependsOnIo(right)
      case Ast.expr.BinOp(left, op, right) =>
        userExprDependsOnIo(left) || userExprDependsOnIo(right)
      case Ast.expr.BoolOp(op, values) =>
        values.exists(v => userExprDependsOnIo(v))
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        userExprDependsOnIo(condition) || userExprDependsOnIo(ifTrue) || userExprDependsOnIo(ifFalse)
      case Ast.expr.Subscript(value, idx) =>
        userExprDependsOnIo(value) || userExprDependsOnIo(idx)
      case a: Ast.expr.Attribute =>
        val t = getArrayItemType(translator.detectType(a))
        if (t == KaitaiStreamType || t == OwnedKaitaiStreamType) {
          true
        } else {
          userExprDependsOnIo(a.value)
        }
      case Ast.expr.Call(func, args) =>
        (func match {
          case Ast.expr.Attribute(value, methodName) =>
            userExprDependsOnIo(value)
        }) || args.exists(v => userExprDependsOnIo(v))
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        values.exists(v => userExprDependsOnIo(v))
      case Ast.expr.CastToType(value, typeName) =>
        userExprDependsOnIo(value)
      case _: Ast.expr.ByteSizeOfType => false
      case _: Ast.expr.BitSizeOfType => false
    }
  }

  def validDependsOnIo(valid: ValidationSpec): Boolean = {
    valid match {
      case ValidationEq(expected) => userExprDependsOnIo(expected)
      case ValidationMin(min) => userExprDependsOnIo(min)
      case ValidationMax(max) => userExprDependsOnIo(max)
      case ValidationRange(min, max) => userExprDependsOnIo(min) || userExprDependsOnIo(max)
      case ValidationAnyOf(values) => values.exists(v => userExprDependsOnIo(v))
      case ValidationInEnum() => false
      case ValidationExpr(expr) => userExprDependsOnIo(expr)
    }
  }

  def attrCheck2(id: Identifier, dataType: DataType, repeat: RepeatSpec, shouldDependOnIo: Option[Boolean], exprTypeOpt: Option[DataType] = None) = {
    val item = Identifier.itemExpr(id, repeat)
    dataType match {
      case ut: UserType =>
        val itemUserType =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[UserType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(true, ut.classSpec.get.name))
          else
            item
        attrUserTypeCheck(id, itemUserType, ut, shouldDependOnIo)
      case t: BytesType =>
        val itemBytes =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[BytesType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(false, Seq("bytes")))
          else
            item
        attrBytesCheck(id, itemBytes, t, shouldDependOnIo)
      case st: StrFromBytesType =>
        val itemStr =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[StrType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(false, Seq("str")))
          else
            item
        val bytes = exprStrToBytes(itemStr, st.encoding)
        attrBytesCheck(id, bytes, st.bytes, shouldDependOnIo)
      case st: SwitchType =>
        attrSwitchCheck(id, st.on, st.cases, repeat, shouldDependOnIo, st.combinedType)
      case _ => // no checks
    }
  }

  def attrValidCheck(attr: AttrLikeSpec, shouldDependOnIo: Option[Boolean]): Unit = {
    attr.valid.foreach { (valid) =>
      typeProvider._currentIteratorType = Some(attr.dataType)
      if (shouldDependOnIo.map(shouldDepend => validDependsOnIo(valid) == shouldDepend).getOrElse(true)) {
        attrValidate(attr, valid, false)
      }
    }
  }

  def attrRepeatExprCheck(id: Identifier, expectedSize: Ast.expr, shouldDependOnIo: Option[Boolean]): Unit = {
    if (shouldDependOnIo.map(shouldDepend => userExprDependsOnIo(expectedSize) != shouldDepend).getOrElse(false))
      return
    attrAssertEqual(
      exprArraySize(Ast.expr.InternalName(id)),
      expectedSize,
      idToMsg(id)
    )
  }

  def attrBytesCheck(id: Identifier, bytes: Ast.expr, t: BytesType, shouldDependOnIoOrig: Option[Boolean]): Unit = {
    val shouldDependOnIo: Option[Boolean] =
      if (t.process.isDefined) {
        if (shouldDependOnIoOrig.getOrElse(true)) {
          None
        } else {
          return
        }
      } else {
        shouldDependOnIoOrig
      }

    val msgId = idToMsg(id)
    val actualSize = exprByteArraySize(bytes)
    val canUseNonIoDependent = shouldDependOnIo.map(shouldDepend => shouldDepend == false).getOrElse(true)
    t match {
      case blt: BytesLimitType => {
        val limitSize = blt.size
        val canUseLimitSize = shouldDependOnIo.map(shouldDepend => userExprDependsOnIo(limitSize) == shouldDepend).getOrElse(true)
        if (canUseLimitSize) {
          if (blt.terminator.isDefined || blt.padRight.isDefined) {
            // size must be "<= declared" (less than or equal to declared size)
            attrAssertLtE(actualSize, limitSize, msgId)
          } else {
            // size must match declared size exactly
            attrAssertEqual(
              actualSize,
              limitSize,
              msgId
            )
          }
        }
        blt.terminator match {
          case Some(terminator) => {
            val term =
              if (terminator.length == 1) {
                terminator.head & 0xff
              } else {
                throw new NotImplementedError("multibyte terminators cannot be serialized yet")
              }
            val actualIndexOfTerm = exprByteArrayIndexOf(bytes, term)
            val isPadRightActive = blt.padRight.map(padByte => padByte != term).getOrElse(false)
            if (!blt.include) {
              if (canUseNonIoDependent) {
                attrAssertEqual(actualIndexOfTerm, Ast.expr.IntNum(-1), msgId)
              }
              if (isPadRightActive && canUseLimitSize) {
                condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Eq, limitSize))
                // check if the last byte is not `pad-right`
                attrBytesPadRightCheck(bytes, actualSize, blt.padRight, msgId)
                condIfFooter
              }
            } else {
              val lastByteIndex = Ast.expr.BinOp(actualSize, Ast.operator.Sub, Ast.expr.IntNum(1))
              if (!isPadRightActive && canUseLimitSize) {
                condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Lt, limitSize))
                // must not be empty (always contains at least the `terminator` byte)
                attrAssertCmp(actualSize, Ast.cmpop.Eq, Ast.expr.IntNum(0), msgId)
                // the user wants to terminate the value prematurely and there's no `pad-right` that
                // could do that, so the last byte of the value must be `terminator`
                attrAssertEqual(actualIndexOfTerm, lastByteIndex, msgId)
                condIfFooter

                condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Eq, limitSize))
                attrTermIncludeCheck(actualIndexOfTerm, lastByteIndex, msgId)
                condIfFooter
              }
              if (isPadRightActive && canUseNonIoDependent) {
                attrTermIncludeCheck(actualIndexOfTerm, lastByteIndex, msgId)

                condIfHeader(Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.Eq, Ast.expr.IntNum(-1)))
                // check if the last byte is not `pad-right`
                attrBytesPadRightCheck(bytes, actualSize, blt.padRight, msgId)
                condIfFooter
              }
            }
          }
          case None =>
            if (canUseLimitSize) {
              // check if the last byte is not `pad-right`
              attrBytesPadRightCheck(bytes, actualSize, blt.padRight, msgId)
            }
        }
      }
      case btt: BytesTerminatedType => {
        if (canUseNonIoDependent) {
          val term =
            if (btt.terminator.length == 1) {
              btt.terminator.head & 0xff
            } else {
              throw new NotImplementedError("multibyte terminators cannot be serialized yet")
            }
          val actualIndexOfTerm = exprByteArrayIndexOf(bytes, term)
          val lastByteIndex: Ast.expr = Ast.expr.BinOp(actualSize, Ast.operator.Sub, Ast.expr.IntNum(1))
          val expectedIndexOfTerm = if (btt.include) {
            if (btt.eosError) {
              // must not be empty (always contains at least the `terminator` byte)
              attrAssertCmp(actualSize, Ast.cmpop.Eq, Ast.expr.IntNum(0), msgId)

              attrAssertEqual(actualIndexOfTerm, lastByteIndex, msgId)
            } else {
              attrTermIncludeCheck(actualIndexOfTerm, lastByteIndex, msgId)
            }
          } else {
            attrAssertEqual(actualIndexOfTerm, Ast.expr.IntNum(-1), msgId)
          }
        }
      }
      case _ => // no checks
    }
  }

  def attrBytesPadRightCheck(bytes: Ast.expr, actualSize: Ast.expr, padRight: Option[Int], msgId: String): Unit =
    padRight.foreach { (padByte) =>
      val lastByte = exprByteArrayLast(bytes)
      attrBasicCheck(
        Ast.expr.BoolOp(
          Ast.boolop.And,
          Seq(
            Ast.expr.Compare(actualSize, Ast.cmpop.NotEq, Ast.expr.IntNum(0)),
            Ast.expr.Compare(
              lastByte,
              Ast.cmpop.Eq,
              Ast.expr.IntNum(padByte)
            )
          )
        ),
        lastByte,
        Ast.expr.IntNum(padByte),
        msgId
      )
    }

  def attrTermIncludeCheck(actualIndexOfTerm: Ast.expr, lastByteIndex: Ast.expr, msgId: String): Unit =
    attrBasicCheck(
      Ast.expr.BoolOp(
        Ast.boolop.And,
        Seq(
          Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.NotEq, Ast.expr.IntNum(-1)),
          Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.NotEq, lastByteIndex)
        )
      ),
      actualIndexOfTerm,
      lastByteIndex,
      msgId
    )

  def attrUserTypeCheck(id: Identifier, utExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean]): Unit = {
    /** @note Must be kept in sync with [[JavaCompiler.parseExpr]] */
    if (!ut.isExternal(typeProvider.nowClass)) {
      // TODO: perhaps it would make sense to enforce `obj._root == obj` and `obj._parent == null`
      // for non-opaque external types in the future, but for now we won't perform any checks on
      // external types
      attrUserTypeParamCheck(id, ut, utExpr, RootIdentifier, CalcKaitaiStructType(), Ast.expr.Name(Ast.identifier(Identifier.ROOT)), shouldDependOnIo)
      attrParentParamCheck(id, Ast.expr.Attribute(utExpr, Ast.identifier(Identifier.PARENT)), ut, shouldDependOnIo)
    }
    if (!ut.isOpaque) {
      (ut.classSpec.get.params, ut.args).zipped.foreach { (paramDef, argExpr) =>
        attrUserTypeParamCheck(id, ut, utExpr, paramDef.id, paramDef.dataType, argExpr, shouldDependOnIo)
      }
    }
  }

  def attrUserTypeParamCheck(id: Identifier, ut: UserType, utExpr: Ast.expr, paramId: Identifier, paramDataType: DataType, argExpr: Ast.expr, shouldDependOnIo: Option[Boolean]): Unit = {
    val paramItemType = getArrayItemType(paramDataType)
    val paramBasedOnIo = (paramItemType == KaitaiStreamType || paramItemType == OwnedKaitaiStreamType)
    // parameters with types `io` or `io[]` never have to be checked for consistency because they're set by the generated code
    if (paramBasedOnIo)
      return
    if (shouldDependOnIo.map(shouldDepend => userExprDependsOnIo(argExpr) != shouldDepend).getOrElse(false))
      return
    val paramAttrName = paramId match {
      case NamedIdentifier(name) => name
      case SpecialIdentifier(name) => name
    }
    val actualArgExpr = Ast.expr.Attribute(utExpr, Ast.identifier(paramAttrName))
    val msgId = idToMsg(id)
    paramDataType match {
      /** @note Must be kept in sync with [[translators.BaseTranslator.translate]] */
      case _: NumericType | _: BooleanType | _: StrType | _: BytesType | _: EnumType =>
        attrAssertEqual(actualArgExpr, argExpr, msgId)
      case _: ArrayType =>
        attrObjectsEqualCheck(actualArgExpr, argExpr, msgId)
      case _: StructType =>
        attrObjectsEqualCheck(actualArgExpr, argExpr, msgId)
      case AnyType =>
        attrObjectsEqualCheck(actualArgExpr, argExpr, msgId)
    }
  }

  def attrSwitchCheck(
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, DataType],
    rep: RepeatSpec,
    shouldDependOnIoOrig: Option[Boolean],
    assignType: DataType
  ): Unit = {
    val shouldDependOnIo: Option[Boolean] =
      if (userExprDependsOnIo(on)) {
        if (shouldDependOnIoOrig.getOrElse(true)) {
          None
        } else {
          return
        }
      } else {
        shouldDependOnIoOrig
      }

    switchCases[DataType](id, on, cases,
      (dataType) => {
        attrCheck2(id, dataType, rep, shouldDependOnIo, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            attrCheck2(RawIdentifier(id), dataType, rep, shouldDependOnIo, Some(assignType))
          case _ =>
            attrCheck2(id, dataType, rep, shouldDependOnIo, Some(assignType))
        }
      } else {
        attrCheck2(id, dataType, rep, shouldDependOnIo, Some(assignType))
      }
    )
  }

  def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit

  // This may turn out to be too Java-specific method, so we can refactor it later.
  def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit

  private
  def idToMsg(id: Identifier): String = id.humanReadable

  def exprByteArraySize(name: Ast.expr) =
    Ast.expr.Attribute(
      name,
      Ast.identifier("size")
    )

  def exprByteArrayLast(name: Ast.expr) =
    Ast.expr.Attribute(
      name,
      Ast.identifier("last")
    )

  def exprByteArrayIndexOf(name: Ast.expr, term: Int) =
    Ast.expr.Call(
      Ast.expr.Attribute(
        name,
        Ast.identifier("index_of")
      ),
      Seq(Ast.expr.IntNum(term))
    )

  def exprStrToBytes(name: Ast.expr, encoding: String) =
    Ast.expr.Call(
      Ast.expr.Attribute(
        name,
        Ast.identifier("to_b")
      ),
      Seq(Ast.expr.Str(encoding))
    )

  def exprArraySize(name: Ast.expr) = exprByteArraySize(name)

  def exprArrayLast(name: Ast.expr) = exprByteArrayLast(name)

  def attrAssertUntilNotEmpty(id: Identifier): Unit = {
    // the array must not be empty (always contains at least the `repeat-until: {true}` element)
    attrAssertCmp(exprArraySize(Ast.expr.InternalName(id)), Ast.cmpop.Eq, Ast.expr.IntNum(0), idToMsg(id))
  }

  def attrAssertUntilCond(id: Identifier, dataType: DataType, repUntil: RepeatUntil, shouldDependOnIo: Option[Boolean]): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    if (shouldDependOnIo.map(shouldDepend => userExprDependsOnIo(repUntil.expr) != shouldDepend).getOrElse(false))
      return
    blockScopeHeader
    handleAssignmentTempVar(
      dataType,
      translator.doName(Identifier.ITERATOR),
      translator.translate(Identifier.itemExpr(id, repUntil))
    )
    attrAssertEqual(
      repUntil.expr,
      Ast.expr.Compare(
        Ast.expr.Name(Ast.identifier(Identifier.INDEX)),
        Ast.cmpop.Eq,
        Ast.expr.BinOp(exprArraySize(Ast.expr.InternalName(id)), Ast.operator.Sub, Ast.expr.IntNum(1))
      ),
      idToMsg(id)
    )
    blockScopeFooter
  }

  def attrIsEofCheck(id: Identifier, expectedIsEof: Boolean, io: String): Unit =
    attrIsEofCheck(io, expectedIsEof, idToMsg(id))

  def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit

  def attrParentParamCheck(id: Identifier, actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean]): Unit =
    attrParentParamCheck(actualParentExpr, ut, shouldDependOnIo, idToMsg(id))

  def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit

  def attrAssertEqual(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.NotEq, expected, msg)

  def attrAssertLtE(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.Gt, expected, msg)

  def attrAssertCmp(actual: Ast.expr, op: Ast.cmpop, expected: Ast.expr, msg: String): Unit =
    attrBasicCheck(
      Ast.expr.Compare(actual, op, expected),
      actual,
      expected,
      msg
    )
}
