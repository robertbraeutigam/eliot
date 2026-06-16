package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{mangleSuffix, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.convertToNestedClassName
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*
import org.objectweb.asm.Opcodes

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
        for {
          _ <- methodGenerator.addLdcInsn[CompilationTypesIO](java.lang.Long.valueOf(integerLiteral.value.toLong))
          _ <- methodGenerator.runNative[CompilationTypesIO](
                 boxFromLong(representationInternalName(uncurriedExpression.expressionType))
               )
        } yield Seq.empty
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
          classes        <- arguments.zipWithIndex.flatTraverse { (expression, idx) =>
                              for {
                                cs <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                _  <- methodGenerator.addCallToApply[CompilationTypesIO]()
                                _  <- methodGenerator
                                        .addCastTo[CompilationTypesIO](NativeType.systemFunctionValue)
                                        .whenA(idx < arguments.size - 1)
                              } yield cs
                            }
          _              <- methodGenerator.addCastTo[CompilationTypesIO](
                              valueType(expectedResultType)
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
        calledVfqn.name.qualifier match {
          case Qualifier.AbilityImplementation(abilityName, _)
              if abilityName.value === "PatternMatch" && calledVfqn.name.name === "handleCases" =>
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
          case Qualifier.AbilityImplementation(abilityName, _)
              if abilityName.value === "TypeMatch" && calledVfqn.name.name === "typeMatch" =>
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
          case _ =>
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
        }
      case FunctionLiteral(parameters, body)                      => ??? // FIXME: applying lambda immediately
      case FunctionApplication(target, arguments2)                => ??? // FIXME: applying on a result function?
    }

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
                                .firstPatternTypeConstructorName(calledVfqn, "PatternMatch")
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
    "PatternMatch$" + dataTypeVfqn.name.name + "$impl"

  /** Emit a backend [[Intrinsics]] call inline. After Phase 3, an `Int[MIN, MAX]` value is carried at the *narrowest*
    * JVM wrapper its range fits (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), so each operation works in the
    * common `long` working representation and (un)boxes at the operand/result representations read from the (already
    * lowered) expression types:
    *   - `integerLiteral[V]` pushes the constant `V` (from its type argument) boxed at the result representation;
    *   - `+`/`-`/`*` unbox both operands to `long`, apply `LADD`/`LSUB`/`LMUL`, and rebox at the result representation;
    *   - `intToString` unboxes its operand to `long` and calls `Long.toString(long)`.
    *
    * This is the JVM specialisation of the Phase-4 leaf natives ("unbox → LADD → narrow → box"); a microcontroller
    * backend would instead pick width-specific instructions from the same lowered representations.
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
    if (calledVfqn === Intrinsics.integerLiteralFQN) {
      typeArgs.headOption match {
        case Some(GroundValue.Direct(value: BigInt, _)) =>
          for {
            _ <- methodGenerator.addLdcInsn[CompilationTypesIO](java.lang.Long.valueOf(value.toLong))
            _ <- methodGenerator.runNative[CompilationTypesIO](boxFromLong(representationInternalName(expectedResultType)))
          } yield Seq.empty
        case _                                          =>
          compilerAbort(sourcedCalledVfqn.as("integerLiteral has no concrete value argument.")).liftToTypes.as(Seq.empty)
      }
    } else if (calledVfqn === Intrinsics.intToStringFQN) {
      val operandRep = representationInternalName(arguments.head.expressionType)
      for {
        classes <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments.head)
        _       <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                     unboxToLong(operandRep)(mv)
                     mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "toString", "(J)Ljava/lang/String;", false)
                   }
      } yield classes
    } else {
      val opcode =
        if (calledVfqn === Intrinsics.plusFQN) Opcodes.LADD
        else if (calledVfqn === Intrinsics.minusFQN) Opcodes.LSUB
        else Opcodes.LMUL // Intrinsics.timesFQN
      val resultRep = representationInternalName(expectedResultType)
      val leftRep   = representationInternalName(arguments(0).expressionType)
      val rightRep  = representationInternalName(arguments(1).expressionType)
      for {
        classes1 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(0))
        _        <- methodGenerator.runNative[CompilationTypesIO](unboxToLong(leftRep))
        classes2 <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, arguments(1))
        _        <- methodGenerator.runNative[CompilationTypesIO](unboxToLong(rightRep))
        _        <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                      mv.visitInsn(opcode)
                      boxFromLong(resultRep)(mv)
                    }
      } yield classes1 ++ classes2
    }
  }

  /** The internal JVM class name (`java/lang/Byte`, …, `java/math/BigInteger`) of a value's machine representation. The
    * ground value must already be lowered (Phase 3), so its head FQN is one of the `Jvm*` representation types.
    */
  private def representationInternalName(t: GroundValue): String =
    NativeType.javaInternalName(valueType(t))

  /** Unbox the boxed integer wrapper of the given representation on the top of the stack into a primitive `long`. All
    * the wrapper classes extend `java.lang.Number`, so `longValue()` widens uniformly.
    */
  private def unboxToLong(repInternalName: String)(mv: org.objectweb.asm.MethodVisitor): Unit =
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, repInternalName, "longValue", "()J", false)

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
      uncurriedMaybe <- getFact(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, arguments.length)).liftToTypes
      resultClasses  <- uncurriedMaybe match
                          case Some(uncurriedValue) =>
                            val parameterTypes = uncurriedValue.parameters.map(p => valueType(p.parameterType))
                            val returnType     = valueType(uncurriedValue.returnType)
                            val methodName     =
                              if (
                                DataClassGenerator
                                  .isConstructor(calledVfqn) || DataClassGenerator.isTypeConstructor(calledVfqn)
                              )
                                calledVfqn.name.name
                              else
                                calledVfqn.name.name + mangleSuffix(typeArgs)
                            for {
                              classes <-
                                arguments.flatTraverse(expression =>
                                  createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                )
                              _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                           calledVfqn,
                                           parameterTypes,
                                           returnType,
                                           Some(methodName)
                                         )
                              _       <- methodGenerator
                                           .addCastTo[CompilationTypesIO](
                                             valueType(expectedResultType)
                                           )
                                           .whenA(valueType(expectedResultType) =!= returnType)
                            } yield classes
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
                           .firstPatternTypeConstructorName(calledVfqn, "TypeMatch")
                           .liftToTypes
      _               <- compilerAbort(
                           sourcedCalledVfqn.as("Could not determine type constructor name for typeMatch.")
                         ).liftToTypes.whenA(constructorName.isEmpty)
      uncurriedMaybe  <- getFact(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, arguments.length)).liftToTypes
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
