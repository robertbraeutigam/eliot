package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addMonomorphicDataFieldsAndCtor, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*

object LambdaGenerator {

  type ExpressionCodeFn =
    (ModuleName, ClassGenerator, MethodGenerator, UncurriedMonomorphicExpression) => CompilationTypesIO[Seq[ClassFile]]

  def generateLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      parameters: Seq[MonomorphicParameterDefinition],
      body: Sourced[UncurriedMonomorphicExpression],
      createExpressionCode: ExpressionCodeFn
  ): CompilationTypesIO[Seq[ClassFile]] =
    parameters match {
      case Seq()           =>
        compilerAbort[Seq[ClassFile]](body.as("Lambda with no parameters cannot be generated.")).liftToTypes
      case Seq(definition) =>
        generateSingleParameterLambda(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          definition,
          body,
          createExpressionCode
        )
      case first +: rest   =>
        // Peel the first parameter into a one-argument closure whose body is the remaining (still curried) lambda.
        // Each frame stays a single-argument `java.util.function.Function`, so the single-parameter generation below
        // handles every nesting level recursively, without inventing an N-ary functional interface.
        val innerType = rest.foldRight(body.value.expressionType) { (parameter, accumulated) =>
          GroundValue.Structure(
            WellKnownTypes.functionDataTypeFQN,
            Seq(parameter.parameterType, accumulated),
            GroundValue.Type
          )
        }
        val innerBody = body.as(UncurriedMonomorphicExpression(innerType, FunctionLiteral(rest, body)))
        generateLambda(moduleName, outerClassGenerator, methodGenerator, Seq(first), innerBody, createExpressionCode)
    }

  private def generateSingleParameterLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      definition: MonomorphicParameterDefinition,
      body: Sourced[UncurriedMonomorphicExpression],
      createExpressionCode: ExpressionCodeFn
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val closedOverNames = body.value.expression.freeVariables
      .filter(_ =!= definition.name.value)
    val returnType      = valueType(body.value.expressionType)

    for {
      closedOverArgs   <- closedOverNames.traverse(getParameterType).map(_.sequence)
      _                <- compilerAbort(body.as("Could not find all types for closed over arguments."))
                            .whenA(closedOverArgs.isEmpty)
                            .liftToTypes
      lambdaIndex      <- incLambdaCount
      methodName       <- getMethodName
      lambdaPrefix      = methodName + "$lambda$"
      lambdaFnParams    = closedOverArgs.get :+ definition
      // Save outer state and set up a fresh TypeState for the lambda body.
      outerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](
                            TypeState(
                              typeMap = lambdaFnParams.map(p => p.name.value -> p).toMap,
                              parameters = lambdaFnParams.map(_.name.value),
                              lambdaCount = outerState.lambdaCount,
                              methodName = methodName
                            )
                          )
      cls1             <-
        outerClassGenerator
          .createMethod[CompilationTypesIO](
            JvmIdentifier.encode(lambdaPrefix + "fn$" + lambdaIndex),
            lambdaFnParams.map(_.parameterType).map(valueType),
            valueType(body.value.expressionType)
          )
          .use { fnGenerator =>
            createExpressionCode(moduleName, outerClassGenerator, fnGenerator, body.value)
          }
      // Restore outer state, preserving lambdaCount from inner (nested lambdas may have incremented it)
      innerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](outerState.copy(lambdaCount = innerState.lambdaCount))
      innerClassWriter <-
        outerClassGenerator
          .createInnerClassGenerator[CompilationTypesIO](
            JvmIdentifier.encode(lambdaPrefix + lambdaIndex),
            Seq("java/util/function/Function")
          )
      _                <- innerClassWriter.addMonomorphicDataFieldsAndCtor[CompilationTypesIO](closedOverArgs.get)
      _                <- innerClassWriter
                            .createApplyMethod[CompilationTypesIO](
                              Seq(valueType(definition.parameterType)),
                              valueType(body.value.expressionType)
                            )
                            .use { applyGenerator =>
                              for {
                                _ <- closedOverArgs.get.traverse_ { argument =>
                                       for {
                                         _ <- applyGenerator
                                                .addLoadVar[CompilationTypesIO](
                                                  ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)),
                                                  0
                                                )
                                         _ <- applyGenerator.addGetField[CompilationTypesIO](
                                                JvmIdentifier.encode(argument.name.value),
                                                valueType(argument.parameterType),
                                                ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default))
                                              )
                                       } yield ()
                                     }
                                _ <- applyGenerator.addLoadVar[CompilationTypesIO](
                                       valueType(definition.parameterType),
                                       1
                                     )
                                _ <- applyGenerator.addCastTo[CompilationTypesIO](
                                       valueType(definition.parameterType)
                                     )
                                _ <- applyGenerator.addCallTo[CompilationTypesIO](
                                       ValueFQN(moduleName, QualifiedName(lambdaPrefix + "fn$" + lambdaIndex, Qualifier.Default)),
                                       lambdaFnParams.map(_.parameterType).map(valueType),
                                       valueType(body.value.expressionType)
                                     )
                              } yield ()
                            }
      classFile        <- innerClassWriter.generate[CompilationTypesIO]()
      _                <- methodGenerator.addNew[CompilationTypesIO](
                            ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default))
                          )
      _                <- closedOverArgs.get.traverse_ { argument =>
                            for {
                              argIndex <- getParameterIndex(argument.name.value)
                              argType  <- getParameterType(argument.name.value)
                              _        <- methodGenerator
                                            .addLoadVar[CompilationTypesIO](valueType(argType.get.parameterType), argIndex.get)
                            } yield ()
                          }
      _                <- methodGenerator.addCallToCtor[CompilationTypesIO](
                            ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)),
                            closedOverArgs.get.map(_.parameterType).map(valueType)
                          )
    } yield classFile +: cls1
  }
}
