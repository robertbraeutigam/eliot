package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.data.IndexedStateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.asm.CommonPatterns.*
import com.vanillasource.eliot.eliotc.jvm.asm.NativeType.types
import com.vanillasource.eliot.eliotc.jvm.asm.{ClassFile, ClassGenerator, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, ResolvedData, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.typesystem.fact.{TypeCheckedFunction, TypedExpression}
import com.vanillasource.eliot.eliotc.typesystem.fact.TypedExpression.*
import com.vanillasource.eliot.eliotc.processor.impl.OneToOneProcessor
import scala.annotation.tailrec

class JvmClassGenerator
    extends OneToOneProcessor((key: GeneratedModule.Key) => GenerateModule.Key(key.moduleName))
    with Logging {

  override def generateFromFact(generateModule: GenerateModule): CompilerIO[Unit] =
    for {
      mainClassGenerator     <- createClassGenerator[CompilerIO](generateModule.moduleName)
      typeFiles              <- generateModule.usedTypes
                                  .filter(stfqn => !types.contains(stfqn.value))
                                  .flatTraverse(sourcedTfqn => createData(mainClassGenerator, sourcedTfqn))
      typeGeneratedFunctions <- generateModule.usedTypes.flatTraverse(collectTypeGeneratedFunctions).map(_.toSet)
      functionFiles          <- generateModule.usedFunctions
                                  .filter(sffqn => !typeGeneratedFunctions.contains(sffqn.value.functionName))
                                  .flatTraverse(sourcedFfqn => createModuleMethod(mainClassGenerator, sourcedFfqn))
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(
                                  GeneratedModule(generateModule.moduleName, typeFiles ++ functionFiles ++ Seq(mainClass))
                                )
    } yield ()

  private def createModuleMethod(mainClassGenerator: ClassGenerator, sourcedFfqn: Sourced[FunctionFQN])(
  : CompilerIO[Seq[ClassFile]] = {
    implementations.get(sourcedFfqn.value) match {
      case Some(nativeImplementation) =>
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        for {
          functionDefinitionMaybe <- getFact(TypeCheckedFunction.Key(sourcedFfqn.value))
          classFiles              <- functionDefinitionMaybe match
                                       case Some(functionDefinition) => createModuleMethod(mainClassGenerator, functionDefinition)
                                       case None                     => compilerError(sourcedFfqn.as(s"Could not find implementation.")).as(Seq.empty)
        } yield classFiles
    }
  }

  private def createModuleMethod(classGenerator: ClassGenerator, functionDefinition: TypeCheckedFunction)(
  : CompilerIO[Seq[ClassFile]] = {
    val parameterTypes = calculateMethodSignature(functionDefinition.definition.body.value)

    classGenerator
      .createMethod[CompilerIO](
        functionDefinition.ffqn.functionName,
        parameterTypes,
        simpleType(functionDefinition.definition.body.value.expressionType)
      )
      .use { methodGenerator =>
        val program = for {
          extractedBody <- extractMethodBody(
                             functionDefinition.definition.name,
                             functionDefinition.definition.body.value.expression,
                             parameterTypes.length + 1
                           )
          classes       <-
            createExpressionCode(functionDefinition.ffqn.moduleName, classGenerator, methodGenerator, extractedBody)
          _             <- debug[CompilationTypesIO](
                             s"From function ${functionDefinition.ffqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                           )
        } yield classes

        program.runA(TypeState())
      }
  }

  /** Extracts parameter arity from curried form.
    */
  // FIXME: does this handle a -> (b -> c) -> d -- needs test
  private def calculateMethodSignature(typedExpression: TypedExpression): Seq[TypeFQN] =
    typedExpression.expression match {
      case FunctionLiteral(parameter, body) =>
        simpleType(parameter.typeReference) +: calculateMethodSignature(body.value)
      case _                                => Seq.empty
    }

  /** Extract method body, expecting the given amount of embedded lambdas.
    */
  private def extractMethodBody(
      sourced: Sourced[?],
      expression: Expression,
      depth: Int
  )(using CompilationProcess): CompilationTypesIO[Expression] =
    if (depth <= 1) {
      expression.pure[CompilationTypesIO]
    } else {
      expression match {
        case FunctionLiteral(parameter, Sourced(_, _, body)) =>
          addParameterDefinition(parameter) >>
            extractMethodBody(sourced, body.expression, depth - 1)
        case _                                               => compilerAbort(sourced.as("Can not extract method body.")).liftToTypes
      }
    }

  private def createExpressionCode(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      expression: Expression
  )(
  ): CompilationTypesIO[Seq[ClassFile]] =
    expression match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.expression,
          Seq(argument.expression)
        ) // One-argument call
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        =>
        methodGenerator.addLdcInsn[CompilationTypesIO](stringLiteral.value).as(Seq.empty)
      case ParameterReference(sourcedParameterName)                            =>
        for {
          index         <- getParameterIndex(sourcedParameterName.value)
          parameterType <- getParameterType(sourcedParameterName.value)
          _             <- compilerAbort(sourcedParameterName.as("Could not find in scope.")).liftToTypes
                             .whenA(index.isEmpty || parameterType.isEmpty)
          _             <- methodGenerator.addLoadVar[CompilationTypesIO](simpleType(parameterType.get.typeReference), index.get)
        } yield Seq.empty
      case ValueReference(Sourced(_, _, ffqn))                                 =>
        // No-argument call
        generateFunctionApplication(moduleName, outerClassGenerator, methodGenerator, expression, Seq.empty)
      case FunctionLiteral(parameter, body)                                    =>
        generateLambda(moduleName, outerClassGenerator, methodGenerator, parameter, body)
    }

  // FIXME: remove this method
  private def calculateMethodSignatureDeprecated(typeReference: TypeReference): Seq[TypeFQN] =
    typeReference match {
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters) if dataType === systemFunctionType =>
        simpleType(genericParameters.head) +: calculateMethodSignatureDeprecated(genericParameters.last)
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters)                                    =>
        Seq(dataType)
      case GenericTypeReference(name, genericParameters)                                                      =>
        Seq(systemAnyType)
    }

  @tailrec
  private def generateFunctionApplication(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      target: Expression,
      arguments: Seq[Expression]
  )(using CompilationProcess): CompilationTypesIO[Seq[ClassFile]] =
    target match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        // Means this is another argument, so just recurse
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.expression,
          arguments.appended(argument.expression)
        )
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        => ???
      case ParameterReference(parameterName)                                   =>
        // Function application on a parameter reference with exactly one parameter?
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
      case ValueReference(sourcedCalledFfqn @ Sourced(_, _, calledFfqn))       =>
        // Calling a function with exactly one argument
        for {
          functionDefinitionMaybe <- getFact(ResolvedFunction.Key(calledFfqn)).liftToTypes
          resultClasses           <- functionDefinitionMaybe match
                                       case Some(functionDefinition) =>
                                         val signatureTypes = calculateMethodSignatureDeprecated(
                                           functionDefinition.definition.valueType
                                         )

                                         for {
                                           classes <-
                                             arguments.flatTraverse(expression =>
                                               createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                             )
                                           _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                                        calledFfqn,
                                                        signatureTypes.init,
                                                        signatureTypes.last
                                                      )
                                         } yield classes
                                       case None                     =>
                                         compilerError(
                                           sourcedCalledFfqn.as("Could not find resolved function."),
                                           Seq(s"Looking for function: ${calledFfqn.show}")
                                         ).liftToTypes.as(Seq.empty)
        } yield resultClasses
      case FunctionLiteral(parameter, body)                                    => ???
    }

  private def generateLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      definition: ArgumentDefinition,
      body: Sourced[TypedExpression]
  )(using CompilationProcess): CompilationTypesIO[Seq[ClassFile]] = {
    val closedOverNames = body.value.expression.toSeq
      .collect { case ParameterReference(parameterName) =>
        parameterName.value
      }
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
