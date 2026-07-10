package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{mangledMethodName, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.convertToNestedClassName
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

object ExpressionCodeGenerator {

  def createExpressionCode(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      uncurriedExpression: UncurriedMonomorphicExpression
  ): CompilationTypesIO[Seq[ClassFile]] =
    uncurriedExpression.expression match {
      case FunctionApplication(target, arguments)           =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.value,
          arguments.map(_.value),
          uncurriedExpression.expressionType
        )
      case IntegerLiteral(integerLiteral)                   =>
        methodGenerator
          .runNative[CompilationTypesIO](
            pushIntegerConstant(integerLiteral.value, representationInternalName(uncurriedExpression.expressionType))
          )
          .as(Seq.empty)
      case StringLiteral(stringLiteral)                     =>
        methodGenerator.addLdcInsn[CompilationTypesIO](stringLiteral.value).as(Seq.empty)
      case ParameterReference(sourcedParameterName)         =>
        for {
          index         <- getParameterIndex(sourcedParameterName.value)
          parameterType <- getParameterType(sourcedParameterName.value)
          _             <- compilerAbort(sourcedParameterName.as("Could not find in scope.")).liftToTypes
                             .whenA(index.isEmpty || parameterType.isEmpty)
          _             <- methodGenerator.addLoadVar[CompilationTypesIO](valueType(parameterType.get.parameterType), index.get)
        } yield Seq.empty
      case MonomorphicValueReference(sourcedVfqn, typeArgs) =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          uncurriedExpression,
          Seq.empty,
          uncurriedExpression.expressionType
        )
      case FunctionLiteral(parameters, body)                =>
        LambdaGenerator.generateLambda(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          parameters,
          body,
          createExpressionCode
        )
    }

  private def generateFunctionApplication(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      typedTarget: UncurriedMonomorphicExpression,
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    typedTarget.expression match {
      case IntegerLiteral(integerLiteral)                         => ???
      case StringLiteral(stringLiteral)                           => ???
      case ParameterReference(parameterName)                      =>
        // Function application on a parameter reference, so this needs to be a Function
        for {
          parameterIndex <- getParameterIndex(parameterName.value)
          parameterType  <- getParameterType(parameterName.value)
          _              <- compilerAbort(parameterName.as("Could not find parameter in scope.")).liftToTypes
                              .whenA(parameterIndex.isEmpty || parameterType.isEmpty)
          _              <- methodGenerator
                              .addLoadVar[CompilationTypesIO](valueType(parameterType.get.parameterType), parameterIndex.get)
          classes        <- applyArgumentsToFunctionValue(
                              moduleName,
                              outerClassGenerator,
                              methodGenerator,
                              arguments,
                              expectedResultType
                            )
        } yield classes
      case MonomorphicValueReference(sourcedCalledVfqn, typeArgs) if Intrinsics.isIntrinsic(sourcedCalledVfqn.value) =>
        generateIntrinsic(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          sourcedCalledVfqn,
          typeArgs,
          arguments,
          expectedResultType
        )
      case MonomorphicValueReference(sourcedCalledVfqn, typeArgs) =>
        val calledVfqn = sourcedCalledVfqn.value
        if (WellKnownTypes.isPatternMatchHandleCases(calledVfqn))
          generatePatternMatchCall(
            moduleName,
            outerClassGenerator,
            methodGenerator,
            sourcedCalledVfqn,
            calledVfqn,
            typeArgs,
            arguments,
            expectedResultType
          )
        else if (WellKnownTypes.isTypeMatchTypeMatch(calledVfqn))
          generateTypeMatchCall(
            moduleName,
            outerClassGenerator,
            methodGenerator,
            sourcedCalledVfqn,
            calledVfqn,
            typeArgs,
            arguments,
            expectedResultType
          )
        else
          generateNormalFunctionCall(
            moduleName,
            outerClassGenerator,
            methodGenerator,
            sourcedCalledVfqn,
            calledVfqn,
            typeArgs,
            arguments,
            expectedResultType
          )
      case FunctionLiteral(parameters, body)                      =>
        // An immediately-applied lambda `(x -> body)(arg)` — a `let`, the shape a non-effectful block `val`/statement
        // lowers to. Generate the lambda as an ordinary closure value, then apply the argument(s) to it exactly as a
        // function-valued parameter is applied. (An effectful block binding is rewritten to `flatMap`/`map` earlier and
        // never reaches here.)
        for {
          lambdaClasses <- LambdaGenerator.generateLambda(
                             moduleName,
                             outerClassGenerator,
                             methodGenerator,
                             parameters,
                             body,
                             createExpressionCode
                           )
          argClasses    <- applyArgumentsToFunctionValue(
                             moduleName,
                             outerClassGenerator,
                             methodGenerator,
                             arguments,
                             expectedResultType
                           )
        } yield lambdaClasses ++ argClasses
      case FunctionApplication(_, _)                              =>
        // Applying the result of another application: the inner application leaves a function value on the stack
        // (its own final cast is to its Function-carrier expression type), the arguments are then applied to it.
        for {
          targetClasses <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, typedTarget)
          argClasses    <- applyArgumentsToFunctionValue(
                             moduleName,
                             outerClassGenerator,
                             methodGenerator,
                             arguments,
                             expectedResultType
                           )
        } yield targetClasses ++ argClasses
    }

  /** Apply arguments one at a time to the function *value* on top of the stack (a `java.util.function.Function`), then
    * cast the final result to the expected type. `apply` returns `Object`, so every intermediate result is cast back to
    * the function interface before absorbing the next argument.
    */
  private def applyArgumentsToFunctionValue(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      classes <- arguments.zipWithIndex.flatTraverse { (expression, idx) =>
                   for {
                     cs <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                     _  <- methodGenerator.addCallToApply[CompilationTypesIO]()
                     _  <- methodGenerator
                             .addCastTo[CompilationTypesIO](NativeType.systemFunctionValue)
                             .whenA(idx < arguments.size - 1)
                   } yield cs
                 }
      _       <- methodGenerator.addCastTo[CompilationTypesIO](valueType(expectedResultType))
    } yield classes

  private def generatePatternMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      typeName             <- ImplementationMarkerUtils
                                .firstPatternTypeConstructorName(calledVfqn, WellKnownTypes.patternMatchAbilityName)
                                .liftToTypes
      dataTypeVfqn          = typeName
                                .map(n => ValueFQN(calledVfqn.moduleName, QualifiedName(n, Qualifier.Default)))
                                .getOrElse(NativeType.systemAnyValue)
      singletonName         = patternMatchSingletonName(dataTypeVfqn)
      singletonVfqn         = ValueFQN(calledVfqn.moduleName, QualifiedName(singletonName, Qualifier.Default))
      singletonInternalName = convertToNestedClassName(singletonVfqn)
      _                    <- methodGenerator.addGetStaticInstance[CompilationTypesIO](
                                singletonInternalName,
                                "L" + singletonInternalName + ";"
                              )
      classes              <- arguments.flatTraverse(expression =>
                                createExpressionCode(
                                  moduleName,
                                  outerClassGenerator,
                                  methodGenerator,
                                  expression
                                )
                              )
      _                    <- methodGenerator.addCallToVirtualMethod[CompilationTypesIO](
                                singletonInternalName,
                                JvmIdentifier.encode("handleCases"),
                                Seq(NativeType.systemAnyValue, NativeType.systemFunctionValue),
                                NativeType.systemAnyValue
                              )
      _                    <- methodGenerator.addCastTo[CompilationTypesIO](valueType(expectedResultType))
    } yield classes

  def patternMatchSingletonName(dataTypeVfqn: ValueFQN): String =
    WellKnownTypes.patternMatchAbilityName + "$" + dataTypeVfqn.name.name + "$impl"

  /** Emit a backend [[Intrinsics]] call inline. After Phase 3, an `Int[MIN, MAX]` value is carried at the *narrowest*
    * JVM wrapper its range fits (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), and the operand/result
    * representations are read from the (already lowered) expression types:
    *   - `intToString` unboxes its operand to `long` and calls `Long.toString(long)`;
    *   - `nativeWiden` converts its operand from the source to the target representation (unbox/rebox, via `BigInteger`
    *     when the target is `BigInteger`);
    *   - an arithmetic leaf (`nativeAdd`/`nativeSubtract`/`nativeMultiply`) computes in primitive `long`
    *     (`LADD`/`LSUB`/`LMUL`, then rebox at the result representation) when operands and result fit `Long`, or in
    *     `java.math.BigInteger` (`add`/`subtract`/`multiply`) when anything overflows it — so a `Long`×`Long` product
    *     whose result range spills into `BigInteger` never truncates through a `long`.
    *
    * This is the JVM realisation of the width-agnostic arithmetic leaves; a microcontroller backend would instead pick
    * width-specific instructions from the same lowered representations.
    */
  private def generateIntrinsic(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      typeArgs: Seq[GroundValue],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val calledVfqn = sourcedCalledVfqn.value
    if (calledVfqn === Intrinsics.intToStringFQN) {
      val operandRep = representationInternalName(arguments.head.expressionType)
      for {
        classes <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments.head)
        _       <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                     unboxToLong(operandRep)(mv)
                     mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "toString", "(J)Ljava/lang/String;", false)
                   }
      } yield classes
    } else if (calledVfqn === Intrinsics.nativeWidenFQN) {
      val sourceRep = representationInternalName(arguments.head.expressionType)
      val targetRep = representationInternalName(expectedResultType)
      for {
        classes <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments.head)
        _       <- methodGenerator.runNative[CompilationTypesIO](convertRepresentation(sourceRep, targetRep))
      } yield classes
    } else if (Intrinsics.boolOps.contains(calledVfqn)) {
      generateBoolIntrinsic(
        moduleName,
        outerClassGenerator,
        methodGenerator,
        calledVfqn,
        arguments,
        expectedResultType
      )
    } else if (calledVfqn === Intrinsics.intLessThanOrEqualFQN) {
      // The ordering leaf: compare in primitive `long` (`LCMP`) when both operands fit it, else via
      // `BigInteger.compareTo`; either way branch the comparison outcome into a boxed `Boolean`.
      val leftRep       = representationInternalName(arguments(0).expressionType)
      val rightRep      = representationInternalName(arguments(1).expressionType)
      val viaBigInteger = leftRep === bigIntegerInternalName || rightRep === bigIntegerInternalName
      val trueLabel     = new Label()
      val endLabel      = new Label()
      for {
        classes1 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _        <- methodGenerator.runNative[CompilationTypesIO](
                      if (viaBigInteger) pushAsBigInteger(leftRep) else unboxToLong(leftRep)
                    )
        classes2 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(1))
        _        <- methodGenerator.runNative[CompilationTypesIO](
                      if (viaBigInteger) pushAsBigInteger(rightRep) else unboxToLong(rightRep)
                    )
        _        <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                      if (viaBigInteger)
                        mv.visitMethodInsn(
                          Opcodes.INVOKEVIRTUAL,
                          bigIntegerInternalName,
                          "compareTo",
                          "(Ljava/math/BigInteger;)I",
                          false
                        )
                      else mv.visitInsn(Opcodes.LCMP)
                      mv.visitJumpInsn(Opcodes.IFLE, trueLabel)
                      pushBoolConstant(false)(mv)
                      mv.visitJumpInsn(Opcodes.GOTO, endLabel)
                      mv.visitLabel(trueLabel)
                      pushBoolConstant(true)(mv)
                      mv.visitLabel(endLabel)
                    }
      } yield classes1 ++ classes2
    } else {
      val resultRep = representationInternalName(expectedResultType)
      val leftRep   = representationInternalName(arguments(0).expressionType)
      val rightRep  = representationInternalName(arguments(1).expressionType)
      // `Long`-range operands and results compute in primitive `long`; anything that touches `BigInteger` (a
      // `BigInteger` operand, or a result that overflowed `Long` — e.g. a `Long`×`Long` product) computes in
      // `BigInteger` so no value is truncated through a `long` round-trip.
      val viaBigInteger =
        resultRep === bigIntegerInternalName || leftRep === bigIntegerInternalName || rightRep === bigIntegerInternalName
      for {
        classes1 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _        <- methodGenerator.runNative[CompilationTypesIO](
                      if (viaBigInteger) pushAsBigInteger(leftRep) else unboxToLong(leftRep)
                    )
        classes2 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(1))
        _        <- methodGenerator.runNative[CompilationTypesIO](
                      if (viaBigInteger) pushAsBigInteger(rightRep) else unboxToLong(rightRep)
                    )
        _        <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                      if (viaBigInteger) bigIntegerOp(calledVfqn)(mv)
                      else {
                        mv.visitInsn(longOpcode(calledVfqn))
                        boxFromLong(resultRep)(mv)
                      }
                    }
      } yield classes1 ++ classes2
    }
  }

  /** Emit a `Bool` primitive/operator inline over the `java.lang.Boolean` representation. Reached only for a genuinely
    * runtime `Bool` — a constant-operand expression is folded away by the compile-time native before codegen.
    *
    *   - `true`/`false` push `Boolean.TRUE`/`FALSE`;
    *   - `!a` unboxes and flips (`ICONST_1`/`IXOR`), reboxing;
    *   - `a && b` / `a || b` unbox both operands and `IAND`/`IOR`, reboxing (both operands are ordinary strict values,
    *     so there is nothing to short-circuit);
    *   - `fold(cond, whenTrue, whenFalse)` branches on `cond` and emits *only the taken arm's* code (an `IFEQ` skip),
    *     so the untaken branch is never evaluated — matching `fold`'s selecting semantics and avoiding the awkward
    *     three-deep stack a strict select would need.
    */
  private def generateBoolIntrinsic(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      calledVfqn: ValueFQN,
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    if (calledVfqn === Intrinsics.boolTrueFQN || calledVfqn === Intrinsics.boolFalseFQN)
      methodGenerator
        .runNative[CompilationTypesIO](pushBoolConstant(calledVfqn === Intrinsics.boolTrueFQN))
        .as(Seq.empty)
    else if (calledVfqn === Intrinsics.boolNotFQN)
      for {
        classes <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _       <- methodGenerator.runNative[CompilationTypesIO](notBool)
      } yield classes
    else if (calledVfqn === Intrinsics.boolAndFQN || calledVfqn === Intrinsics.boolOrFQN) {
      val opcode = if (calledVfqn === Intrinsics.boolAndFQN) Opcodes.IAND else Opcodes.IOR
      for {
        classes1 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _        <- methodGenerator.runNative[CompilationTypesIO](unboxBool)
        classes2 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(1))
        _        <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                      unboxBool(mv)
                      mv.visitInsn(opcode)
                      boxBool(mv)
                    }
      } yield classes1 ++ classes2
    } else { // boolFoldFQN
      val elseLabel = new Label()
      val endLabel  = new Label()
      for {
        condClasses  <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _            <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                          unboxBool(mv)
                          mv.visitJumpInsn(Opcodes.IFEQ, elseLabel)
                        }
        trueClasses  <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(1))
        _            <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                          mv.visitJumpInsn(Opcodes.GOTO, endLabel)
                          mv.visitLabel(elseLabel)
                        }
        falseClasses <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(2))
        _            <- methodGenerator.runNative[CompilationTypesIO](_.visitLabel(endLabel))
        _            <- methodGenerator.addCastTo[CompilationTypesIO](valueType(expectedResultType))
      } yield condClasses ++ trueClasses ++ falseClasses
    }

  private def unboxBool(mv: MethodVisitor): Unit =
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)

  private def boxBool(mv: MethodVisitor): Unit =
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)

  private def notBool(mv: MethodVisitor): Unit = {
    unboxBool(mv)
    mv.visitInsn(Opcodes.ICONST_1)
    mv.visitInsn(Opcodes.IXOR)
    boxBool(mv)
  }

  private def pushBoolConstant(value: Boolean)(mv: MethodVisitor): Unit =
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/Boolean", if (value) "TRUE" else "FALSE", "Ljava/lang/Boolean;")

  /** The primitive `long` opcode for an arithmetic leaf FQN. */
  private def longOpcode(leafVfqn: ValueFQN): Int =
    if (leafVfqn === Intrinsics.nativeAddFQN) Opcodes.LADD
    else if (leafVfqn === Intrinsics.nativeSubtractFQN) Opcodes.LSUB
    else Opcodes.LMUL // nativeMultiply

  /** The `java.math.BigInteger` instance method for an arithmetic leaf FQN; applied to the two `BigInteger`s on the
    * stack, it leaves the (already-boxed) `BigInteger` result.
    */
  private def bigIntegerOp(leafVfqn: ValueFQN)(mv: org.objectweb.asm.MethodVisitor): Unit = {
    val method =
      if (leafVfqn === Intrinsics.nativeAddFQN) "add"
      else if (leafVfqn === Intrinsics.nativeSubtractFQN) "subtract"
      else "multiply" // nativeMultiply
    mv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      bigIntegerInternalName,
      method,
      "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
      false
    )
  }

  private val bigIntegerInternalName = "java/math/BigInteger"

  /** The internal JVM class name (`java/lang/Byte`, …, `java/math/BigInteger`) of a value's machine representation. The
    * ground value must already be lowered (Phase 3), so its head FQN is one of the `Jvm*` representation types.
    */
  private def representationInternalName(t: GroundValue): String =
    NativeType.javaInternalName(valueType(t))

  /** Convert the boxed value on the stack from its source representation to its target representation, preserving the
    * logical integer (the caller guarantees it fits the target). Routes through `BigInteger` when the target is
    * `BigInteger`, otherwise through primitive `long`.
    */
  private def convertRepresentation(sourceRep: String, targetRep: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    if (sourceRep === targetRep) ()                              // same representation: the value already has the right form
    else if (targetRep === bigIntegerInternalName) pushAsBigInteger(sourceRep)(mv)
    else {
      unboxToLong(sourceRep)(mv)
      boxFromLong(targetRep)(mv)
    }

  /** Leave a `java.math.BigInteger` on the stack from a boxed value of the given representation: a `BigInteger` is
    * already in the right form; any narrower wrapper is unboxed to `long` and lifted via `BigInteger.valueOf`.
    */
  private def pushAsBigInteger(repInternalName: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    if (repInternalName =!= bigIntegerInternalName) {
      unboxToLong(repInternalName)(mv)
      mv.visitMethodInsn(Opcodes.INVOKESTATIC, bigIntegerInternalName, "valueOf", "(J)Ljava/math/BigInteger;", false)
    }

  /** Unbox the boxed integer wrapper of the given representation on the top of the stack into a primitive `long`. All
    * the wrapper classes extend `java.lang.Number`, so `longValue()` widens uniformly.
    */
  private def unboxToLong(repInternalName: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, repInternalName, "longValue", "()J", false)

  /** Push an integer constant boxed at the given machine representation. A `BigInteger` representation is built at full
    * precision via `new BigInteger(decimalString)`, so a materialised constant whose value exceeds `Long` range is not
    * truncated; every narrower wrapper goes through a `long` constant and [[boxFromLong]].
    */
  private def pushIntegerConstant(value: BigInt, repInternalName: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    if (repInternalName === bigIntegerInternalName) {
      mv.visitTypeInsn(Opcodes.NEW, bigIntegerInternalName)
      mv.visitInsn(Opcodes.DUP)
      mv.visitLdcInsn(value.toString)
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, bigIntegerInternalName, "<init>", "(Ljava/lang/String;)V", false)
    } else {
      mv.visitLdcInsn(java.lang.Long.valueOf(value.toLong))
      boxFromLong(repInternalName)(mv)
    }

  /** Box the primitive `long` on the top of the stack into the wrapper of the given representation, narrowing first
    * (`l2i` + `i2b`/`i2s` for `Byte`/`Short`) so the boxed value matches the declared descriptor.
    */
  private def boxFromLong(repInternalName: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    repInternalName match {
      case "java/lang/Byte"       =>
        mv.visitInsn(Opcodes.L2I)
        mv.visitInsn(Opcodes.I2B)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false)
      case "java/lang/Short"      =>
        mv.visitInsn(Opcodes.L2I)
        mv.visitInsn(Opcodes.I2S)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false)
      case "java/lang/Integer"    =>
        mv.visitInsn(Opcodes.L2I)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
      case "java/math/BigInteger" =>
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/math/BigInteger", "valueOf", "(J)Ljava/math/BigInteger;", false)
      case _                      => // java/lang/Long
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)
    }

  private def generateNormalFunctionCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      // An application spine can be longer than the callee's natural arity (`unwrap(w)("x")` on a 1-parameter
      // accessor): the direct call absorbs `naturalArity` arguments, and the excess is applied one at a time to the
      // function value it returns. Body-less natives have no natural arity and keep the full spine.
      monomorphicMaybe          <- getFactIfProduced(MonomorphicValue.Key(calledVfqn, typeArgs)).liftToTypes
      directCallArity            = monomorphicMaybe.flatMap(_.naturalArity).fold(arguments.length)(_ min arguments.length)
      (directArgs, overApplied)  = arguments.splitAt(directCallArity)
      uncurriedMaybe            <- getFactIfProduced(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, directArgs.length)).liftToTypes
      resultClasses             <- uncurriedMaybe match
                          case Some(uncurriedValue) =>
                            val returnType = valueType(uncurriedValue.returnType)
                            val methodName =
                              if (
                                DataClassGenerator
                                  .isConstructor(calledVfqn) || DataClassGenerator.isTypeConstructor(calledVfqn)
                              )
                                calledVfqn.name.name
                              else
                                mangledMethodName(calledVfqn, typeArgs)
                            for {
                              // A value constructor is emitted once and shared by every instantiation, so its bare
                              // type-parameter fields erase to `Object` here exactly as on the definition side — the call
                              // descriptor must match the single shared factory (DataClassGenerator.erasePolymorphicFields).
                              parameters    <-
                                if (DataClassGenerator.isConstructor(calledVfqn))
                                  getFactOrAbort(OperatorResolvedValue.Key(calledVfqn)).liftToTypes
                                    .map(DataClassGenerator.erasePolymorphicFields(_, uncurriedValue.parameters))
                                else uncurriedValue.parameters.pure[CompilationTypesIO]
                              parameterTypes = parameters.map(p => valueType(p.parameterType))
                              classes       <-
                                directArgs.flatTraverse(expression =>
                                  createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                )
                              _             <- methodGenerator.addCallTo[CompilationTypesIO](
                                                 calledVfqn,
                                                 parameterTypes,
                                                 returnType,
                                                 Some(methodName)
                                               )
                              overClasses   <-
                                if (overApplied.isEmpty)
                                  methodGenerator
                                    .addCastTo[CompilationTypesIO](valueType(expectedResultType))
                                    .whenA(valueType(expectedResultType) =!= returnType)
                                    .as(Seq.empty[ClassFile])
                                else
                                  methodGenerator
                                    .addCastTo[CompilationTypesIO](NativeType.systemFunctionValue)
                                    .whenA(returnType =!= NativeType.systemFunctionValue) >>
                                    applyArgumentsToFunctionValue(
                                      moduleName,
                                      outerClassGenerator,
                                      methodGenerator,
                                      overApplied,
                                      expectedResultType
                                    )
                            } yield classes ++ overClasses
                          case None                 =>
                            compilerError(
                              sourcedCalledVfqn.as("Could not find uncurried function."),
                              Seq(
                                s"Looking for function: ${calledVfqn.show} with type args (${typeArgs.size} args)"
                              )
                            ).liftToTypes.as(Seq.empty)
    } yield resultClasses

  private def generateTypeMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: GroundValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      constructorName <- ImplementationMarkerUtils
                           .firstPatternTypeConstructorName(calledVfqn, WellKnownTypes.typeMatchAbilityName)
                           .liftToTypes
      _               <- compilerAbort(
                           sourcedCalledVfqn.as("Could not determine type constructor name for typeMatch.")
                         ).liftToTypes.whenA(constructorName.isEmpty)
      uncurriedMaybe  <- getFactIfProduced(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, arguments.length)).liftToTypes
      classes         <- uncurriedMaybe match {
                           case Some(uncurriedValue) =>
                             val parameterTypes = uncurriedValue.parameters.map(p => valueType(p.parameterType))
                             val returnType     = valueType(uncurriedValue.returnType)
                             for {
                               classes <- arguments.flatTraverse(expression =>
                                            createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                          )
                               _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                            calledVfqn,
                                            parameterTypes,
                                            returnType,
                                            Some("typeMatch$" + constructorName.get)
                                          )
                               _       <- methodGenerator
                                            .addCastTo[CompilationTypesIO](valueType(expectedResultType))
                                            .whenA(valueType(expectedResultType) =!= returnType)
                             } yield classes
                           case None                 =>
                             compilerError(
                               sourcedCalledVfqn.as("Could not find uncurried typeMatch function."),
                               Seq(s"Looking for function: ${calledVfqn.show}")
                             ).liftToTypes.as(Seq.empty)
                         }
    } yield classes
}
