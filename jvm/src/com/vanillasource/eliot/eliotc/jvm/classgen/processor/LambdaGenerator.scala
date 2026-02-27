package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedExpression.*

object LambdaGenerator {

  type ExpressionCodeFn =
    (ModuleName, ClassGenerator, MethodGenerator, UncurriedExpression) => CompilationTypesIO[Seq[ClassFile]]

  def generateLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      parameters: Seq[ParameterDefinition],
      body: Sourced[UncurriedExpression],
      createExpressionCode: ExpressionCodeFn
  ): CompilationTypesIO[Seq[ClassFile]] = {
    if (parameters.length > 1) {
      ??? // Multi-parameter lambdas not currently supported
    }

    val definition      = parameters.headOption.getOrElse(???)
    val closedOverNames = collectParameterReferences(body.value.expression)
      .filter(_ =!= definition.name.value)
    val returnType      = simpleType(body.value.expressionType)

    for {
      closedOverArgs   <- closedOverNames.traverse(getParameterType).map(_.sequence)
      _                <- compilerAbort(body.as("Could not find all types for closed over arguments."))
                            .whenA(closedOverArgs.isEmpty)
                            .liftToTypes
      lambdaIndex      <- incLambdaCount
      methodName       <- getMethodName
      lambdaPrefix      = methodName + "$lambda$"
      lambdaFnParams    = closedOverArgs.get ++ parameters
      // Save outer state and set up a fresh TypeState for the lambda body.
      outerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](TypeState(
                            typeMap = lambdaFnParams.map(p => p.name.value -> p).toMap,
                            parameters = lambdaFnParams.map(_.name.value),
                            lambdaCount = outerState.lambdaCount,
                            methodName = methodName
                          ))
      cls1             <-
        outerClassGenerator
          .createMethod[CompilationTypesIO](
            JvmIdentifier.encode(lambdaPrefix + "fn$" + lambdaIndex),
            lambdaFnParams.map(_.parameterType).map(simpleType),
            simpleType(body.value.expressionType)
          )
          .use { fnGenerator =>
            createExpressionCode(moduleName, outerClassGenerator, fnGenerator, body.value)
          }
      // Restore outer state, preserving lambdaCount from inner (nested lambdas may have incremented it)
      innerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](outerState.copy(lambdaCount = innerState.lambdaCount))
      innerClassWriter <-
        outerClassGenerator
          .createInnerClassGenerator[CompilationTypesIO](JvmIdentifier.encode(lambdaPrefix + lambdaIndex), Seq("java/util/function/Function"))
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilationTypesIO](closedOverArgs.get)
      _                <- innerClassWriter
                            .createApplyMethod[CompilationTypesIO](
                              Seq(simpleType(definition.parameterType)),
                              simpleType(body.value.expressionType)
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
                                                simpleType(argument.parameterType),
                                                ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default))
                                              )
                                       } yield ()
                                     }
                                _ <- applyGenerator.addLoadVar[CompilationTypesIO](
                                       simpleType(definition.parameterType),
                                       1
                                     )
                                _ <- applyGenerator.addCastTo[CompilationTypesIO](
                                       simpleType(definition.parameterType)
                                     )
                                _ <- applyGenerator.addCallTo[CompilationTypesIO](
                                       ValueFQN(moduleName, QualifiedName(lambdaPrefix + "fn$" + lambdaIndex, Qualifier.Default)),
                                       lambdaFnParams.map(_.parameterType).map(simpleType),
                                       simpleType(body.value.expressionType)
                                     )
                              } yield ()
                            }
      classFile        <- innerClassWriter.generate[CompilationTypesIO]()
      _                <- methodGenerator.addNew[CompilationTypesIO](ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)))
      _                <- closedOverArgs.get.traverse_ { argument =>
                            for {
                              argIndex <- getParameterIndex(argument.name.value)
                              argType  <- getParameterType(argument.name.value)
                              _        <- methodGenerator
                                            .addLoadVar[CompilationTypesIO](simpleType(argType.get.parameterType), argIndex.get)
                            } yield ()
                          }
      _                <- methodGenerator.addCallToCtor[CompilationTypesIO](
                            ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)),
                            closedOverArgs.get.map(_.parameterType).map(simpleType)
                          )
    } yield classFile +: cls1
  }

  def collectParameterReferences(expr: Expression): Seq[String] =
    expr match {
      case FunctionApplication(target, arguments) =>
        collectParameterReferences(target.value.expression) ++
          arguments.flatMap(arg => collectParameterReferences(arg.value.expression))
      case FunctionLiteral(parameters, body)      =>
        collectParameterReferences(body.value.expression)
      case IntegerLiteral(_)                      => Seq.empty
      case StringLiteral(_)                       => Seq.empty
      case ParameterReference(parameterName)      => Seq(parameterName.value)
      case ValueReference(_)                      => Seq.empty
    }
}
