package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.IndexedStateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.types
import NativeImplementation.implementations
import TypeState.*
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor2, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemUnitValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry2.fact.*
import com.vanillasource.eliot.eliotc.uncurry2.fact.UncurriedExpression.*
import com.vanillasource.eliot.eliotc.used2.UsedNames
import com.vanillasource.eliot.eliotc.used2.UsedNames.UsageStats

class JvmClassGenerator extends SingleKeyTypeProcessor[GeneratedModule.Key] with Logging {

  override protected def generateFact(key: GeneratedModule.Key): CompilerIO[Unit] =
    for {
      usedNames          <- getFactOrAbort(UsedNames.Key(key.vfqn))
      usedValues          = usedNames.usedNames.filter((vfqn, _) => vfqn.moduleName === key.moduleName)
      mainClassGenerator <- createClassGenerator[CompilerIO](key.moduleName)
      functionFiles      <- usedValues.toSeq.flatTraverse { case (vfqn, stats) =>
                              createModuleMethod(mainClassGenerator, vfqn, stats)
                            }
      mainClass          <- mainClassGenerator.generate[CompilerIO]()
      _                  <- registerFactIfClear(GeneratedModule(key.moduleName, key.vfqn, functionFiles ++ Seq(mainClass)))
    } yield ()

  private def selectBestArity(stats: UsageStats): Int =
    if (stats.directCallApplications.isEmpty) {
      0
    } else {
      stats.directCallApplications.maxByOption(_._2).map(_._1).getOrElse(0)
    }

  private def createModuleMethod(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats
  ): CompilerIO[Seq[ClassFile]] = {
    implementations.get(vfqn) match {
      case Some(nativeImplementation) =>
        // There's a native implementation for this method so get it
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        // Not a native method, should have a body and generate it
        val arity = selectBestArity(stats)
        for {
          uncurriedValue <- getFactOrAbort(UncurriedValue.Key(vfqn, arity))
          classFiles     <- createModuleMethod(mainClassGenerator, uncurriedValue)
          _              <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(uncurriedValue))
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedValue
  ): CompilerIO[Seq[ClassFile]] = {
    uncurriedValue.body match {
      case Some(body) =>
        classGenerator
          .createMethod[CompilerIO](
            uncurriedValue.vfqn.name,
            uncurriedValue.parameters.map(p => simpleType(p.parameterType)),
            simpleType(uncurriedValue.returnType)
          )
          .use { methodGenerator =>
            val bodyExpression = UncurriedExpression(uncurriedValue.returnType, body.value)
            val program        = for {
              // Add parameters to state
              _       <- uncurriedValue.parameters.traverse_(addParameterDefinition)
              // Generate code for the body
              classes <-
                createExpressionCode(
                  uncurriedValue.vfqn.moduleName,
                  classGenerator,
                  methodGenerator,
                  bodyExpression
                )
              _       <-
                debug[CompilationTypesIO](
                  s"From function ${uncurriedValue.vfqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                )
            } yield classes

            program.runA(TypeState())
          }
      case None       =>
        compilerAbort(uncurriedValue.name.as(s"Function not implemented."))
    }
  }

  private def createExpressionCode(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      uncurriedExpression: UncurriedExpression
  ): CompilationTypesIO[Seq[ClassFile]] =
    uncurriedExpression.expression match {
      case FunctionApplication(target, arguments)   =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.value,
          arguments.map(_.value),
          uncurriedExpression.expressionType
        )
      case IntegerLiteral(integerLiteral)           => ???
      case StringLiteral(stringLiteral)             =>
        methodGenerator.addLdcInsn[CompilationTypesIO](stringLiteral.value).as(Seq.empty)
      case ParameterReference(sourcedParameterName) =>
        for {
          index         <- getParameterIndex(sourcedParameterName.value)
          parameterType <- getParameterType(sourcedParameterName.value)
          _             <- compilerAbort(sourcedParameterName.as("Could not find in scope.")).liftToTypes
                             .whenA(index.isEmpty || parameterType.isEmpty)
          _             <- methodGenerator.addLoadVar[CompilationTypesIO](simpleType(parameterType.get.parameterType), index.get)
        } yield Seq.empty
      case ValueReference(sourcedVfqn)              =>
        // This is practically a zero argument function call to this
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          uncurriedExpression,
          Seq.empty,
          uncurriedExpression.expressionType
        )
      case FunctionLiteral(parameters, body)        =>
        generateLambda(moduleName, outerClassGenerator, methodGenerator, parameters, body)
    }

  private def generateFunctionApplication(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      typedTarget: UncurriedExpression,
      arguments: Seq[UncurriedExpression],
      expectedResultType: ExpressionValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    typedTarget.expression match {
      case IntegerLiteral(integerLiteral)          => ??? // FIXME: we can't apply functions on this, right?
      case StringLiteral(stringLiteral)            => ??? // FIXME: we can't apply functions on this, right?
      case ParameterReference(parameterName)       =>
        // Function application on a parameter reference, so this needs to be a Function
        // FIXME: This only works with 1-arguments now, since this needs to be a java.lang.Function
        for {
          parameterIndex <- getParameterIndex(parameterName.value)
          parameterType  <- getParameterType(parameterName.value)
          _              <- compilerAbort(parameterName.as("Could not find parameter in scope.")).liftToTypes
                              .whenA(parameterIndex.isEmpty || parameterType.isEmpty)
          _              <- methodGenerator
                              .addLoadVar[CompilationTypesIO](simpleType(parameterType.get.parameterType), parameterIndex.get)
          // FIXME: this does not work when currying and it fails runtime, it is not detected here
          classes        <- arguments.flatTraverse(expression =>
                              createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                            )
          _              <- methodGenerator.addCallToApply[CompilationTypesIO]()
          _              <- methodGenerator.addCastTo[CompilationTypesIO](
                              simpleType(expectedResultType)
                            )
        } yield classes
      case ValueReference(sourcedCalledVfqn)       =>
        // Calling a function
        val calledVfqn = sourcedCalledVfqn.value
        for {
          usedNamesMaybe  <- getFact(UsedNames.Key(calledVfqn)).liftToTypes
          calledArity      = usedNamesMaybe
                               .flatMap(_.usedNames.get(calledVfqn))
                               .map(stats => selectBestArity(stats))
                               .getOrElse(arguments.length)
          uncurriedMaybe  <- getFact(UncurriedValue.Key(calledVfqn, calledArity)).liftToTypes
          resultClasses   <- uncurriedMaybe match
                               case Some(uncurriedValue) =>
                                 val parameterTypes =
                                   uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                                 val returnType     = simpleType(uncurriedValue.returnType)
                                 // FIXME: this doesn't seem to check whether arguments match either
                                 for {
                                   classes <-
                                     arguments.flatTraverse(expression =>
                                       createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                     )
                                   _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                                calledVfqn,
                                                parameterTypes,
                                                returnType
                                              )
                                   _       <- methodGenerator
                                                .addCastTo[CompilationTypesIO](
                                                  simpleType(expectedResultType)
                                                )
                                                .whenA(simpleType(expectedResultType) =!= returnType)
                                 } yield classes
                               case None                 =>
                                 compilerError(
                                   sourcedCalledVfqn.as("Could not find uncurried function."),
                                   Seq(s"Looking for function: ${calledVfqn.show}")
                                 ).liftToTypes.as(Seq.empty)
        } yield resultClasses
      case FunctionLiteral(parameters, body)       => ??? // FIXME: applying lambda immediately
      case FunctionApplication(target, arguments2) => ??? // FIXME: applying on a result function?
    }

  private def collectParameterReferences(expr: Expression): Seq[String] =
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

  private def generateLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      parameters: Seq[ParameterDefinition],
      body: Sourced[UncurriedExpression]
  ): CompilationTypesIO[Seq[ClassFile]] = {
    // Validate parameter count - only support 0 or 1 parameter
    if (parameters.length > 1) {
      ??? // Multi-parameter lambdas not currently supported
    }

    val definition      = parameters.headOption.getOrElse(???) // Should have exactly 1 parameter
    val closedOverNames = collectParameterReferences(body.value.expression)
      .filter(_ =!= definition.name.value)
    val returnType      = simpleType(body.value.expressionType)

    for {
      _                <- addParameterDefinition(definition)
      closedOverArgs   <- closedOverNames.traverse(getParameterType).map(_.sequence)
      _                <- compilerAbort(body.as("Could not find all types for closed over arguments."))
                            .whenA(closedOverArgs.isEmpty)
                            .liftToTypes
      lambdaIndex      <- incLambdaCount
      cls1             <-
        outerClassGenerator
          .createMethod[CompilationTypesIO](
            "lambdaFn$" + lambdaIndex,
            closedOverArgs.get.map(_.parameterType).map(simpleType),
            simpleType(body.value.expressionType)
          )
          .use { fnGenerator =>
            createExpressionCode(moduleName, outerClassGenerator, fnGenerator, body.value)
          }
      innerClassWriter <-
        outerClassGenerator
          .createInnerClassGenerator[CompilationTypesIO]("lambda$" + lambdaIndex, Seq("java/util/function/Function"))
      _                <- innerClassWriter.addDataFieldsAndCtor2[CompilationTypesIO](closedOverArgs.get)
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
                                                  ValueFQN(moduleName, "lambda$" + lambdaIndex),
                                                  0 // The data object is the parameter
                                                )
                                         _ <- applyGenerator.addGetField[CompilationTypesIO](
                                                argument.name.value,
                                                simpleType(argument.parameterType),
                                                ValueFQN(moduleName, "lambda$" + lambdaIndex)
                                              )
                                       } yield ()
                                     }
                                // Call the static lambdaFn
                                _ <- applyGenerator.addCallTo[CompilationTypesIO](
                                       ValueFQN(moduleName, "lambdaFn$" + lambdaIndex),
                                       closedOverArgs.get.map(_.parameterType).map(simpleType),
                                       simpleType(body.value.expressionType)
                                     )
                              } yield ()
                            }
      classFile        <- innerClassWriter.generate[CompilationTypesIO]()
      _                <- methodGenerator.addNew[CompilationTypesIO](ValueFQN(moduleName, "lambda$" + lambdaIndex))
      _                <- closedOverArgs.get.traverse_ { argument =>
                            for {
                              argIndex <- getParameterIndex(argument.name.value)
                              argType  <- getParameterType(argument.name.value)
                              _        <- methodGenerator
                                            .addLoadVar[CompilationTypesIO](simpleType(argType.get.parameterType), argIndex.get)
                            } yield ()
                          }
      _                <- methodGenerator.addCallToCtor[CompilationTypesIO]( // Call constructor
                            ValueFQN(moduleName, "lambda$" + lambdaIndex),
                            closedOverArgs.get.map(_.parameterType).map(simpleType)
                          )
      // FIXME: add apply: calling the static method
    } yield classFile +: cls1
  }

  /** Create a JVM compatible static main, if this method is the eliot main, presumably generated from the
    * JvmProgramGenerator.
    */
  private def createApplicationMain(mainVfqn: ValueFQN, generator: ClassGenerator): CompilerIO[Unit] =
    generator.createMainMethod[CompilerIO]().use { methodGenerator =>
      methodGenerator.addCallTo(mainVfqn, Seq.empty, systemUnitValue)
    }

  /** @return
    *   Iff the method definition is a suitable main to run from the JVM
    */
  private def isMain(uncurriedValue: UncurriedValue): Boolean =
    uncurriedValue.name.value === "main" &&
      uncurriedValue.parameters.isEmpty &&
      (uncurriedValue.returnType match {
        case ExpressionValue.ConcreteValue(value) =>
          import com.vanillasource.eliot.eliotc.eval.fact.Value
          value match {
            case Value.Structure(fields, _) =>
              fields.get("$typeName") match {
                case Some(Value.Direct(vfqn: ValueFQN, _)) =>
                  vfqn.moduleName.packages === Seq("eliot", "lang") &&
                  vfqn.moduleName.name === "Unit" &&
                  vfqn.name === "Unit"
                case _                                     => false
              }
            case _                          => false
          }
        case _                                    => false
      })
}
