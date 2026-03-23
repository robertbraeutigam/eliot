package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{mangleSuffix, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.convertToNestedClassName
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
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
          _ <- methodGenerator.runNative[CompilationTypesIO] { mv =>
                 mv.visitMethodInsn(
                   Opcodes.INVOKESTATIC,
                   "java/lang/Long",
                   "valueOf",
                   "(J)Ljava/lang/Long;",
                   false
                 )
               }
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
      expectedResultType: Value
  ): CompilationTypesIO[Seq[ClassFile]] =
    typedTarget.expression match {
      case IntegerLiteral(integerLiteral)                    => ???
      case StringLiteral(stringLiteral)                      => ???
      case ParameterReference(parameterName)                 =>
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
          case Qualifier.AbilityImplementation(abilityName, params)
              if abilityName.value === "TypeMatch" && calledVfqn.name.name === "typeMatch" =>
            generateTypeMatchCall(
              moduleName,
              outerClassGenerator,
              methodGenerator,
              sourcedCalledVfqn,
              calledVfqn,
              params,
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
      case FunctionLiteral(parameters, body)                 => ??? // FIXME: applying lambda immediately
      case FunctionApplication(target, arguments2)           => ??? // FIXME: applying on a result function?
    }

  private def generatePatternMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      typeArgs: Seq[Value],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: Value
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val dataTypeVfqn          = calledVfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(_, params) =>
        findTypeName(params)
          .map(name => ValueFQN(calledVfqn.moduleName, QualifiedName(name, Qualifier.Default)))
          .getOrElse(NativeType.systemAnyValue)
      case _                                          => NativeType.systemAnyValue
    }
    val singletonName         = patternMatchSingletonName(dataTypeVfqn)
    val singletonVfqn         = ValueFQN(calledVfqn.moduleName, QualifiedName(singletonName, Qualifier.Default))
    val singletonInternalName = convertToNestedClassName(singletonVfqn)
    for {
      _       <- methodGenerator.addGetStaticInstance[CompilationTypesIO](
                   singletonInternalName,
                   "L" + singletonInternalName + ";"
                 )
      classes <- arguments.flatTraverse(expression =>
                   createExpressionCode(
                     moduleName,
                     outerClassGenerator,
                     methodGenerator,
                     expression
                   )
                 )
      _       <- methodGenerator.addCallToVirtualMethod[CompilationTypesIO](
                   singletonInternalName,
                   JvmIdentifier.encode("handleCases"),
                   Seq(NativeType.systemAnyValue, NativeType.systemFunctionValue),
                   NativeType.systemAnyValue
                 )
      _       <- methodGenerator.addCastTo[CompilationTypesIO](valueType(expectedResultType))
    } yield classes
  }

  def patternMatchSingletonName(dataTypeVfqn: ValueFQN): String =
    "PatternMatch$" + dataTypeVfqn.name.name + "$impl"

  private def generateNormalFunctionCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      typeArgs: Seq[Value],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: Value
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      uncurriedMaybe <- getFact(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, arguments.length)).liftToTypes
      resultClasses  <- uncurriedMaybe match
                          case Some(uncurriedValue) =>
                            val parameterTypes = uncurriedValue.parameters.map(p => valueType(p.parameterType))
                            val returnType     = valueType(uncurriedValue.returnType)
                            val methodName     =
                              if (DataClassGenerator.isConstructor(calledVfqn) || DataClassGenerator.isTypeConstructor(calledVfqn))
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
                                s"Looking for function: ${calledVfqn.show} with type args ${typeArgs.map(_.show).mkString(",")}"
                              )
                            ).liftToTypes.as(Seq.empty)
    } yield resultClasses

  private def generateTypeMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      qualifierParams: Seq[com.vanillasource.eliot.eliotc.core.fact.Expression],
      typeArgs: Seq[Value],
      arguments: Seq[UncurriedMonomorphicExpression],
      expectedResultType: Value
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val constructorName = findTypeName(qualifierParams)
    for {
      _              <- compilerAbort(
                          sourcedCalledVfqn.as("Could not determine type constructor name for typeMatch.")
                        ).liftToTypes.whenA(constructorName.isEmpty)
      uncurriedMaybe <- getFact(UncurriedMonomorphicValue.Key(calledVfqn, typeArgs, arguments.length)).liftToTypes
      classes        <- uncurriedMaybe match {
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

  private def findTypeName(params: Seq[com.vanillasource.eliot.eliotc.core.fact.Expression]): Option[String] = {
    import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression}
    def find(expr: CoreExpression): Option[String] =
      expr match {
        case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type => Some(qn.value.name)
        case CoreExpression.FunctionApplication(target, _) => find(target.value)
        case _                                             => None
      }
    params.headOption.flatMap(find)
  }
}
