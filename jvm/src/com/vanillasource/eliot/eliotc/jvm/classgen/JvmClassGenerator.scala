package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.data.{IndexedStateT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.asm.{ClassFile, ClassGenerator, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.asm.NativeType.types
import com.vanillasource.eliot.eliotc.jvm.classgen.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType, systemUnitType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.jvm.asm.CommonPatterns._
import TypeState._

import scala.annotation.tailrec

class JvmClassGenerator
    extends OneToOneProcessor((key: GeneratedModule.Key) => GenerateModule.Key(key.moduleName))
    with Logging {

  override def generateFromFact(generateModule: GenerateModule)(using CompilationProcess): IO[Unit] = {
    val program = for {
      mainClassGenerator     <- createClassGenerator[CompilationIO](generateModule.moduleName)
      typeFiles              <- generateModule.usedTypes
                                  .filter(stfqn => !types.contains(stfqn.value))
                                  .flatTraverse(sourcedTfqn => createData(mainClassGenerator, sourcedTfqn))
      typeGeneratedFunctions <- generateModule.usedTypes.flatTraverse(collectTypeGeneratedFunctions).map(_.toSet)
      functionFiles          <- generateModule.usedFunctions
                                  .filter(sffqn => !typeGeneratedFunctions.contains(sffqn.value.functionName))
                                  .flatTraverse(sourcedFfqn => createModuleMethod(mainClassGenerator, sourcedFfqn))
      mainClass              <- mainClassGenerator.generate[CompilationIO]()
      _                      <- (debug[IO](s"Generated ${generateModule.moduleName.show}, with type files: ${typeFiles
                                    .map(_.fileName)
                                    .mkString(", ")}, with function files: ${functionFiles.map(_.fileName).mkString(", ")}") >> registerFact(
                                  GeneratedModule(generateModule.moduleName, typeFiles ++ functionFiles ++ Seq(mainClass))
                                )).liftIfNoErrors
    } yield ()

    program.runCompilation_()
  }

  private def createModuleMethod(mainClassGenerator: ClassGenerator, sourcedFfqn: Sourced[FunctionFQN])(using
      CompilationProcess
  ): CompilationIO[Seq[ClassFile]] = {
    implementations.get(sourcedFfqn.value) match {
      case Some(nativeImplementation) =>
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty).liftToCompilationIO
      case None                       =>
        for {
          functionDefinitionMaybe <- getFact(TypeCheckedFunction.Key(sourcedFfqn.value)).liftToCompilationIO
          classFiles              <- functionDefinitionMaybe match
                                       case Some(functionDefinition) => createModuleMethod(mainClassGenerator, functionDefinition)
                                       case None                     => compilerError(sourcedFfqn.as(s"Could not find implementation.")).as(Seq.empty)
        } yield classFiles
    }
  }

  private def createModuleMethod(classGenerator: ClassGenerator, functionDefinition: TypeCheckedFunction)(using
      CompilationProcess
  ): CompilationIO[Seq[ClassFile]] = {
    val signatureTypes = calculateMethodSignature(functionDefinition.definition.valueType)

    classGenerator
      .createMethod[CompilationIO](functionDefinition.ffqn.functionName, signatureTypes.init, signatureTypes.last)
      .use { methodGenerator =>
        val program = for {
          extractedBody <- extractMethodBody(
                             functionDefinition.definition.name,
                             functionDefinition.definition.body.get.value,
                             signatureTypes.length
                           )
          classes       <- createExpressionCode(classGenerator, methodGenerator, extractedBody)
          _             <- debug[CompilationTypesIO](
                             s"From function ${functionDefinition.ffqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                           )
        } yield classes

        program.runA(TypeState())
      }
  }

  /** Extracts parameter arity from curried form.
    */
  private def calculateMethodSignature(typeReference: TypeReference): Seq[TypeFQN] =
    // FIXME: does this handle a -> (b -> c) -> d -- needs test
    typeReference match {
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters) if dataType === systemFunctionType =>
        simpleType(genericParameters.head) +: calculateMethodSignature(genericParameters.last)
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters)                                    =>
        Seq(dataType)
      case GenericTypeReference(name, genericParameters)                                                      =>
        Seq(systemAnyType)
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
            extractMethodBody(sourced, body, depth - 1)
        case _                                               => compilerAbort(sourced.as("Can not extract method body.")).liftToTypes
      }
    }

  private def createExpressionCode(
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      expression: Expression
  )(using
      CompilationProcess
  ): CompilationTypesIO[Seq[ClassFile]] =
    expression match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        generateFunctionApplication(outerClassGenerator, methodGenerator, target, Seq(argument)) // One-argument call
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
        generateFunctionApplication(outerClassGenerator, methodGenerator, expression, Seq.empty)
      case FunctionLiteral(parameter, body)                                    =>
        generateLambda(outerClassGenerator, parameter, body)
    }

  @tailrec
  private def generateFunctionApplication(
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      target: Expression,
      arguments: Seq[Expression]
  )(using CompilationProcess): CompilationTypesIO[Seq[ClassFile]] =
    target match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        // Means this is another argument, so just recurse
        generateFunctionApplication(outerClassGenerator, methodGenerator, target, arguments.appended(argument))
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        => ???
      case ParameterReference(parameterName)                                   => ???
      case ValueReference(sourcedCalledFfqn @ Sourced(_, _, calledFfqn))       =>
        // Calling a function with exactly one argument
        for {
          functionDefinitionMaybe <- getFact(ResolvedFunction.Key(calledFfqn)).liftToCompilationIO.liftToTypes
          resultClasses           <- functionDefinitionMaybe match
                                       case Some(functionDefinition) =>
                                         val signatureTypes = calculateMethodSignature(functionDefinition.definition.valueType)

                                         for {
                                           classes <- arguments.flatTraverse(expression =>
                                                        createExpressionCode(outerClassGenerator, methodGenerator, expression)
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
      outerClassGenerator: ClassGenerator,
      definition: ArgumentDefinition,
      body: Sourced[Expression]
  )(using CompilationProcess): CompilationTypesIO[Seq[ClassFile]] = {
    val closedOverNames = body.value.toSeq
      .collect { case ParameterReference(parameterName) =>
        parameterName.value
      }
      .filter(_ =!= definition.name.value)

    for {
      _              <- addParameterDefinition(definition)
      closedOverArgs <- closedOverNames.traverse(getParameterType).map(_.sequence)
      _              <- compilerAbort(body.as("Could not find all types for closed over arguments."))
                          .whenA(closedOverArgs.isEmpty)
                          .liftToTypes
      lambdaIndex    <- incLambdaCount
      cls1           <-
        outerClassGenerator
          .createMethod[CompilationTypesIO](
            "lambdaFn$" + lambdaIndex,
            closedOverArgs.get.map(_.typeReference).map(simpleType),
            systemAnyType
          )
          .use { fnGenerator => createExpressionCode(outerClassGenerator, fnGenerator, body.value) }
      cls2           <- createDataClass(outerClassGenerator, "lambda$" + lambdaIndex, closedOverArgs.get).liftToTypes
      // FIXME: add logic to inner class + add instantiation to main class
      // FIXME: Class needs to extend Function, needs apply(a)
      // FIXME: apply needs to call a static method here with all parameters
      // FIXME: Need to instantiate Function
    } yield cls1 ++ cls2
  }

  private def createData(
      outerClassGenerator: ClassGenerator,
      sourcedTfqn: Sourced[TypeFQN]
  )(using CompilationProcess): CompilationIO[Seq[ClassFile]] =
    for {
      typeDefinitionMaybe <- getFact(ResolvedData.Key(sourcedTfqn.value)).liftToCompilationIO
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
                                       .createMethod[CompilationIO](
                                         sourcedTfqn.value.typeName, // TODO: is name legal?
                                         typeDefinition.definition.fields.get.map(_.typeReference).map(simpleType),
                                         sourcedTfqn.value
                                       )
                                       .use { methodGenerator =>
                                         for {
                                           // Allocate new data object
                                           _ <- methodGenerator.addNew[CompilationIO](sourcedTfqn.value)
                                           // Load constructor arguments
                                           _ <- typeDefinition.definition.fields.get.zipWithIndex.traverse_ {
                                                  (fieldDefinition, index) =>
                                                    methodGenerator
                                                      .addLoadVar[CompilationIO](simpleType(fieldDefinition.typeReference), index)
                                                }
                                           // Call constructor
                                           _ <- methodGenerator.addCallToCtor[CompilationIO](
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
                                             .createMethod[CompilationIO](
                                               argumentDefinition.name.value,
                                               Seq(sourcedTfqn.value),
                                               simpleType(argumentDefinition.typeReference)
                                             )
                                             .use { accessorGenerator =>
                                               for {
                                                 _ <- accessorGenerator
                                                        .addLoadVar[CompilationIO](
                                                          sourcedTfqn.value,
                                                          0 // The data object is the parameter
                                                        )
                                                 _ <- accessorGenerator.addGetField[CompilationIO](
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
      fields: Seq[ArgumentDefinition]
  )(using CompilationProcess): CompilationIO[Seq[ClassFile]] =
    for {
      innerClassWriter <- outerClassGenerator.createInnerClassGenerator[CompilationIO](innerClassName)
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilationIO](fields)
      classFile        <- innerClassWriter.generate[CompilationIO]()
    } yield Seq(classFile)

  private def collectTypeGeneratedFunctions(
      sourcedTfqn: Sourced[TypeFQN]
  )(using CompilationProcess): CompilationIO[Seq[String]] =
    getFact(ResolvedData.Key(sourcedTfqn.value)).liftToCompilationIO.map {
      case Some(resolvedData) =>
        resolvedData.definition.fields.getOrElse(Seq.empty).map(_.name.value) ++ Seq(sourcedTfqn.value.typeName)
      case None               => Seq(sourcedTfqn.value.typeName)
    }
}
