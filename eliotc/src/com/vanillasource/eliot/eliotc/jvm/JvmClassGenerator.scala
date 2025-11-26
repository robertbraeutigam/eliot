package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.CatsAsm.*
import com.vanillasource.eliot.eliotc.jvm.GeneratedModule.ClassFile
import com.vanillasource.eliot.eliotc.jvm.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedData, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import scala.annotation.tailrec

class JvmClassGenerator extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case GenerateModule(moduleName, usedFunctions, usedTypes) => generateModule(moduleName, usedFunctions, usedTypes)
      case _                                                    => IO.unit

  private def generateModule(
      moduleName: ModuleName,
      usedFunctions: Seq[Sourced[FunctionFQN]],
      usedTypes: Seq[Sourced[TypeFQN]]
  )(using
      process: CompilationProcess
  ): IO[Unit] = {
    (for {
      mainClassGenerator <- createClassGenerator(moduleName).liftToCompilationIO
      typeFiles          <- usedTypes.flatTraverse(sourcedTfqn => createData(mainClassGenerator, sourcedTfqn))
      functionFiles      <- usedFunctions.flatTraverse(sourcedFfqn => createModuleMethod(mainClassGenerator, sourcedFfqn))
      mainClass          <- mainClassGenerator.generate().liftToCompilationIO
      _                  <- process
                              .registerFact(GeneratedModule(moduleName, typeFiles ++ functionFiles ++ Seq(mainClass)))
                              .liftIfNoErrors
    } yield ()).runCompilation_()
  }

  private def createModuleMethod(mainClassGenerator: ClassGenerator, sourcedFfqn: Sourced[FunctionFQN])(using
      process: CompilationProcess
  ): CompilationIO[Seq[ClassFile]] =
    for {
      functionDefinitionMaybe <- process.getFact(TypeCheckedFunction.Key(sourcedFfqn.value)).liftToCompilationIO
      _                       <- functionDefinitionMaybe match {
                                   case Some(functionDefinition) =>
                                     createModuleMethod(mainClassGenerator, functionDefinition)
                                   case None                     =>
                                     implementations.get(sourcedFfqn.value) match {
                                       case Some(nativeImplementation) =>
                                         nativeImplementation.generateMethod(mainClassGenerator).liftToCompilationIO
                                       case None                       => compilerError(sourcedFfqn.as(s"Could not find implementation."))
                                     }
                                 }
    } yield Seq.empty

  private def createModuleMethod(classGenerator: ClassGenerator, functionDefinition: TypeCheckedFunction)(using
      CompilationProcess
  ): CompilationIO[Unit] = {
    val signatureTypes = calculateMethodSignature(functionDefinition.definition.valueType)

    classGenerator.createMethod[CompilationIO](functionDefinition.ffqn.functionName, signatureTypes).use {
      methodGenerator =>
        createExpressionCode(
          methodGenerator,
          extractMethodBody(functionDefinition.definition.body.get.value, signatureTypes.length)
        )
    }
  }

  /** Extracts parameter arity from curried form.
    */
  private def calculateMethodSignature(typeReference: TypeReference): Seq[TypeFQN] =
    typeReference match {
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters) if dataType === systemFunctionType =>
        calculateMethodSignature(genericParameters.head) ++ calculateMethodSignature(genericParameters.last)
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters)                                    =>
        Seq(dataType)
      case GenericTypeReference(name, genericParameters)                                                      =>
        Seq(systemAnyType)
    }

  /** Extract method body, expecting the given amount of embedded lambdas.
    */
  @tailrec
  private def extractMethodBody(expression: Expression, depth: Int): Expression =
    if (depth <= 1) {
      expression
    } else {
      expression match {
        case FunctionApplication(target, argument)           => throw new IllegalStateException("Can not extract method body.")
        case IntegerLiteral(integerLiteral)                  => throw new IllegalStateException("Can not extract method body.")
        case StringLiteral(stringLiteral)                    => throw new IllegalStateException("Can not extract method body.")
        case ParameterReference(parameterName)               => throw new IllegalStateException("Can not extract method body.")
        case ValueReference(valueName)                       => throw new IllegalStateException("Can not extract method body.")
        case FunctionLiteral(parameter, Sourced(_, _, body)) => extractMethodBody(body, depth - 1)
      }
    }

  private def createExpressionCode(methodGenerator: MethodGenerator, expression: Expression)(using
      CompilationProcess
  ): CompilationIO[Unit] =
    expression match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        generateFunctionApplication(methodGenerator, target, Seq(argument)) // One-argument call
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        =>
        methodGenerator.addLdcInsn(stringLiteral.value)
      case ParameterReference(parameterName)                                   => ???
      case ValueReference(Sourced(_, _, ffqn))                                 =>
        // No-argument call
        generateFunctionApplication(methodGenerator, expression, Seq.empty)
      case FunctionLiteral(parameter, body)                                    => ???
    }

  @tailrec
  private def generateFunctionApplication(
      methodGenerator: MethodGenerator,
      target: Expression,
      arguments: Seq[Expression]
  )(using process: CompilationProcess): CompilationIO[Unit] =
    target match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        // Means this is another argument, so just recurse
        generateFunctionApplication(methodGenerator, target, arguments.appended(argument))
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        => ???
      case ParameterReference(parameterName)                                   => ???
      case ValueReference(sourcedCalledFfqn @ Sourced(_, _, calledFfqn))       =>
        // Calling a function with exactly one argument
        for {
          functionDefinitionMaybe <- process.getFact(ResolvedFunction.Key(calledFfqn)).liftToCompilationIO
          _                       <- functionDefinitionMaybe match
                                       case Some(functionDefinition) =>
                                         for {
                                           _ <- arguments.traverse_(expression => createExpressionCode(methodGenerator, expression))
                                           _ <- methodGenerator.addCallTo[CompilationIO](
                                                  calledFfqn,
                                                  calculateMethodSignature(functionDefinition.definition.valueType)
                                                )
                                         } yield ()
                                       case None                     =>
                                         compilerError(sourcedCalledFfqn.as(s"Could not find resolved ${calledFfqn.show}"))
        } yield ()
      case FunctionLiteral(parameter, body)                                    => ???
    }

  private def createData(
      mainClassGenerator: ClassGenerator,
      sourcedTfqn: Sourced[TypeFQN]
  )(using process: CompilationProcess): CompilationIO[Seq[ClassFile]] =
    for {
      // Define the data object
      classWriter         <- mainClassGenerator.createInnerClassGenerator(sourcedTfqn.value.typeName).liftToCompilationIO
      typeDefinitionMaybe <- process.getFact(ResolvedData.Key(sourcedTfqn.value)).liftToCompilationIO
      _                   <- typeDefinitionMaybe match {
                               case Some(typeDefinition) =>
                                 // Define the inner data container
                                 typeDefinition.definition.fields.traverse_ { argumentDefinition =>
                                   argumentDefinition.typeReference match {
                                     case DirectTypeReference(dataType, genericParameters) =>
                                       classWriter.createField[CompilationIO](argumentDefinition.name.value, dataType.value)
                                     case GenericTypeReference(name, genericParameters)    =>
                                       classWriter.createField[CompilationIO](argumentDefinition.name.value, systemAnyType)
                                   }
                                 }
                               // TODO: define constructor function
                               // TODO: define accessors
                               case None                 =>
                                 compilerError(
                                   sourcedTfqn.as(
                                     s"Could not find type resolved ${TypeFQN.fullyQualified.show(sourcedTfqn.value)}"
                                   )
                                 )
                             }
      classFile           <- classWriter.generate().liftToCompilationIO
    } yield Seq(classFile)
}
