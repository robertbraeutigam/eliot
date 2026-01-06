package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.IndexedStateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.types
import NativeImplementation.implementations
import TypeState.*
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, ResolvedData}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry.UncurriedTypedExpression.*
import com.vanillasource.eliot.eliotc.uncurry.{UncurriedFunction, UncurriedTypedExpression}
import com.vanillasource.eliot.eliotc.used.UsedSymbols

class JvmClassGenerator extends SingleKeyTypeProcessor[GeneratedModule.Key] with Logging {

  override protected def generateFact(key: GeneratedModule.Key): CompilerIO[Unit] =
    for {
      usedSymbols            <- getFactOrAbort(UsedSymbols.Key(key.ffqn))
      usedFunctions           = usedSymbols.usedFunctions.filter(_.value.moduleName == key.moduleName)
      usedTypes               = usedSymbols.usedTypes.filter(_.value.moduleName == key.moduleName)
      mainClassGenerator     <- createClassGenerator[CompilerIO](key.moduleName)
      typeFiles              <- usedTypes
                                  .filter(stfqn => !types.contains(stfqn.value))
                                  .flatTraverse(sourcedTfqn => createData(mainClassGenerator, sourcedTfqn))
      typeGeneratedFunctions <- usedTypes.flatTraverse(collectTypeGeneratedFunctions).map(_.toSet)
      functionFiles          <- usedFunctions
                                  .filter(sffqn => !typeGeneratedFunctions.contains(sffqn.value.functionName))
                                  .flatTraverse(sourcedFfqn => createModuleMethod(mainClassGenerator, sourcedFfqn))
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(GeneratedModule(key.moduleName, key.ffqn, typeFiles ++ functionFiles ++ Seq(mainClass)))
    } yield ()

  private def createModuleMethod(
      mainClassGenerator: ClassGenerator,
      sourcedFfqn: Sourced[FunctionFQN]
  ): CompilerIO[Seq[ClassFile]] = {
    implementations.get(sourcedFfqn.value) match {
      case Some(nativeImplementation) =>
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        for {
          functionDefinition <- getFactOrAbort(UncurriedFunction.Key(sourcedFfqn.value))
          classFiles         <- createModuleMethod(mainClassGenerator, functionDefinition)
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      functionDefinition: UncurriedFunction
  ): CompilerIO[Seq[ClassFile]] = {
    functionDefinition.definition.body match {
      case Some(body) =>
        // Function with body - generate code
        // Validate parameter count
        if (functionDefinition.definition.parameters.length > 1) {
          return compilerError(
            functionDefinition.definition.name.as(
              s"Functions with more than one parameter are not currently supported. Found ${functionDefinition.definition.parameters.length} parameters."
            )
          ).as(Seq.empty)
        }

        val parameterTypes = functionDefinition.definition.parameters.map(p => simpleType(p.typeReference))

        classGenerator
          .createMethod[CompilerIO](
            functionDefinition.ffqn.functionName,
            parameterTypes,
            simpleType(functionDefinition.definition.returnType)
          )
          .use { methodGenerator =>
            val program = for {
              // Add parameters to state
              _       <- functionDefinition.definition.parameters.traverse_(addParameterDefinition)
              // Generate code for the body
              classes <-
                createExpressionCode(
                  functionDefinition.ffqn.moduleName,
                  classGenerator,
                  methodGenerator,
                  body.value.expression
                )
              _       <- debug[CompilationTypesIO](
                           s"From function ${functionDefinition.ffqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                         )
            } yield classes

            program.runA(TypeState())
          }
      case None       =>
        // Function without body - skip code generation (likely abstract/native)
        Seq.empty.pure[CompilerIO]
    }
  }

  private def createExpressionCode(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      expression: Expression
  ): CompilationTypesIO[Seq[ClassFile]] =
    expression match {
      case FunctionApplication(target, arguments)   =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.value.expression,
          arguments.map(_.value.expression)
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
          _             <- methodGenerator.addLoadVar[CompilationTypesIO](simpleType(parameterType.get.typeReference), index.get)
        } yield Seq.empty
      case ValueReference(Sourced(_, _, ffqn))      =>
        // No-argument call
        generateFunctionApplication(moduleName, outerClassGenerator, methodGenerator, expression, Seq.empty)
      case FunctionLiteral(parameters, body)        =>
        generateLambda(moduleName, outerClassGenerator, methodGenerator, parameters, body)
    }

  private def generateFunctionApplication(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      target: Expression,
      arguments: Seq[Expression]
  ): CompilationTypesIO[Seq[ClassFile]] = {
    // Validate argument count - only support 0 or 1 argument
    if (arguments.length > 1) {
      ??? // Multi-argument function applications not currently supported
    }

    target match {
      case IntegerLiteral(integerLiteral)                                => ???
      case StringLiteral(stringLiteral)                                  => ???
      case ParameterReference(parameterName)                             =>
        // Function application on a parameter reference
        for {
          parameterIndex <- getParameterIndex(parameterName.value)
          parameterType  <- getParameterType(parameterName.value)
          _              <- compilerAbort(parameterName.as("Could not find parameter in scope.")).liftToTypes
                              .whenA(parameterIndex.isEmpty || parameterType.isEmpty)
          _              <- methodGenerator
                              .addLoadVar[CompilationTypesIO](simpleType(parameterType.get.typeReference), parameterIndex.get)
          classes        <- arguments.flatTraverse(expression =>
                              createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                            )
          _              <- methodGenerator.addCallToApply[CompilationTypesIO]()
        } yield classes
      case ValueReference(sourcedCalledFfqn @ Sourced(_, _, calledFfqn)) =>
        // Calling a function
        for {
          uncurriedFunctionMaybe <- getFact(UncurriedFunction.Key(calledFfqn)).liftToTypes
          resultClasses          <- uncurriedFunctionMaybe match
                                      case Some(uncurriedFunction) =>
                                        val parameterTypes =
                                          uncurriedFunction.definition.parameters.map(p => simpleType(p.typeReference))
                                        val returnType     = simpleType(uncurriedFunction.definition.returnType)

                                        for {
                                          classes <-
                                            arguments.flatTraverse(expression =>
                                              createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                            )
                                          _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                                       calledFfqn,
                                                       parameterTypes,
                                                       returnType
                                                     )
                                        } yield classes
                                      case None                    =>
                                        compilerError(
                                          sourcedCalledFfqn.as("Could not find uncurried function."),
                                          Seq(s"Looking for function: ${calledFfqn.show}")
                                        ).liftToTypes.as(Seq.empty)
        } yield resultClasses
      case FunctionLiteral(parameters, body)                             => ???
      case FunctionApplication(target, arguments)                        => ???
    }
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
      parameters: Seq[ArgumentDefinition],
      body: Sourced[UncurriedTypedExpression]
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
            closedOverArgs.get.map(_.typeReference).map(simpleType),
            simpleType(body.value.expressionType)
          )
          .use { fnGenerator =>
            createExpressionCode(moduleName, outerClassGenerator, fnGenerator, body.value.expression)
          }
      innerClassWriter <-
        outerClassGenerator
          .createInnerClassGenerator[CompilationTypesIO]("lambda$" + lambdaIndex, Seq("java/util/function/Function"))
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilationTypesIO](closedOverArgs.get)
      _                <- innerClassWriter
                            .createApplyMethod[CompilationTypesIO](
                              Seq(simpleType(definition.typeReference)),
                              simpleType(body.value.expressionType)
                            )
                            .use { applyGenerator =>
                              for {
                                _ <- closedOverArgs.get.traverse_ { argument =>
                                       for {
                                         _ <- applyGenerator
                                                .addLoadVar[CompilationTypesIO](
                                                  TypeFQN(moduleName, "lambda$" + lambdaIndex),
                                                  0 // The data object is the parameter
                                                )
                                         _ <- applyGenerator.addGetField[CompilationTypesIO](
                                                argument.name.value,
                                                simpleType(argument.typeReference),
                                                TypeFQN(moduleName, "lambda$" + lambdaIndex)
                                              )
                                       } yield ()
                                     }
                                // Call the static lambdaFn
                                _ <- applyGenerator.addCallTo[CompilationTypesIO](
                                       FunctionFQN(moduleName, "lambdaFn$" + lambdaIndex),
                                       closedOverArgs.get.map(_.typeReference).map(simpleType),
                                       simpleType(body.value.expressionType)
                                     )
                              } yield ()
                            }
      classFile        <- innerClassWriter.generate[CompilationTypesIO]()
      _                <- methodGenerator.addNew[CompilationTypesIO](TypeFQN(moduleName, "lambda$" + lambdaIndex))
      _                <- closedOverArgs.get.traverse_ { argument =>
                            for {
                              argIndex <- getParameterIndex(argument.name.value)
                              argType  <- getParameterType(argument.name.value)
                              _        <- methodGenerator
                                            .addLoadVar[CompilationTypesIO](simpleType(argType.get.typeReference), argIndex.get)
                            } yield ()
                          }
      _                <- methodGenerator.addCallToCtor[CompilationTypesIO]( // Call constructor
                            TypeFQN(moduleName, "lambda$" + lambdaIndex),
                            closedOverArgs.get.map(_.typeReference).map(simpleType)
                          )
      // FIXME: add apply: calling the static method
    } yield classFile +: cls1
  }

  private def createData(
      outerClassGenerator: ClassGenerator,
      sourcedTfqn: Sourced[TypeFQN]
  ): CompilerIO[Seq[ClassFile]] =
    for {
      typeDefinitionMaybe <- getFact(ResolvedData.Key(sourcedTfqn.value))
      classes             <- typeDefinitionMaybe match {
                               case Some(typeDefinition) =>
                                 for {
                                   _  <- compilerAbort(sourcedTfqn.as("Type not fully defined."))
                                           .unlessA(typeDefinition.definition.fields.isDefined)
                                   cs <-
                                     createDataClass(
                                       outerClassGenerator,
                                       sourcedTfqn.value.typeName,
                                       typeDefinition.definition.fields.get
                                     )
                                   // Define data function
                                   _  <-
                                     outerClassGenerator
                                       .createMethod[CompilerIO](
                                         sourcedTfqn.value.typeName, // TODO: is name legal?
                                         typeDefinition.definition.fields.get.map(_.typeReference).map(simpleType),
                                         sourcedTfqn.value
                                       )
                                       .use { methodGenerator =>
                                         for {
                                           // Allocate new data object
                                           _ <- methodGenerator.addNew[CompilerIO](sourcedTfqn.value)
                                           // Load constructor arguments
                                           _ <- typeDefinition.definition.fields.get.zipWithIndex.traverse_ {
                                                  (fieldDefinition, index) =>
                                                    methodGenerator
                                                      .addLoadVar[CompilerIO](simpleType(fieldDefinition.typeReference), index)
                                                }
                                           // Call constructor
                                           _ <- methodGenerator.addCallToCtor[CompilerIO](
                                                  sourcedTfqn.value,
                                                  typeDefinition.definition.fields.get
                                                    .map(_.typeReference)
                                                    .map(simpleType)
                                                )
                                         } yield ()
                                       }
                                   // Define accessors
                                   _  <- typeDefinition.definition.fields.get.traverse_ { argumentDefinition =>
                                           outerClassGenerator
                                             .createMethod[CompilerIO](
                                               argumentDefinition.name.value,
                                               Seq(sourcedTfqn.value),
                                               simpleType(argumentDefinition.typeReference)
                                             )
                                             .use { accessorGenerator =>
                                               for {
                                                 _ <- accessorGenerator
                                                        .addLoadVar[CompilerIO](
                                                          sourcedTfqn.value,
                                                          0 // The data object is the parameter
                                                        )
                                                 _ <- accessorGenerator.addGetField[CompilerIO](
                                                        argumentDefinition.name.value,
                                                        simpleType(argumentDefinition.typeReference),
                                                        sourcedTfqn.value
                                                      )
                                               } yield ()
                                             }
                                         }

                                 } yield cs
                               case None                 => compilerError(sourcedTfqn.as("Could not find resolved type.")).as(Seq.empty)
                             }
    } yield classes

  private def createDataClass(
      outerClassGenerator: ClassGenerator,
      innerClassName: String,
      fields: Seq[ArgumentDefinition],
      javaInterfaces: Seq[String] = Seq.empty
  ): CompilerIO[Seq[ClassFile]] =
    for {
      innerClassWriter <- outerClassGenerator.createInnerClassGenerator[CompilerIO](innerClassName, javaInterfaces)
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilerIO](fields)
      classFile        <- innerClassWriter.generate[CompilerIO]()
    } yield Seq(classFile)

  private def collectTypeGeneratedFunctions(
      sourcedTfqn: Sourced[TypeFQN]
  ): CompilerIO[Seq[String]] =
    getFact(ResolvedData.Key(sourcedTfqn.value)).map {
      case Some(resolvedData) =>
        resolvedData.definition.fields.getOrElse(Seq.empty).map(_.name.value) ++ Seq(sourcedTfqn.value.typeName)
      case None               => Seq(sourcedTfqn.value.typeName)
    }
}
