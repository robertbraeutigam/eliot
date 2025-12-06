package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.CatsAsm.*
import com.vanillasource.eliot.eliotc.jvm.GeneratedModule.ClassFile
import com.vanillasource.eliot.eliotc.jvm.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.NativeType.{javaSignatureName, types}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType, systemUnitType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedData, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import org.objectweb.asm.Opcodes

import scala.annotation.tailrec

class JvmClassGenerator
    extends OneToOneProcessor((key: GeneratedModule.Key) => GenerateModule.Key(key.moduleName))
    with Logging {

  override def generateFromFact(generateModule: GenerateModule)(using process: CompilationProcess): IO[Unit] = {
    val program = for {
      mainClassGenerator     <- createClassGenerator(generateModule.moduleName).liftToCompilationIO
      typeFiles              <- generateModule.usedTypes
                                  .filter(stfqn => !types.contains(stfqn.value))
                                  .flatTraverse(sourcedTfqn => createData(mainClassGenerator, sourcedTfqn))
      typeGeneratedFunctions <- generateModule.usedTypes.flatTraverse(collectTypeGeneratedFunctions).map(_.toSet)
      functionFiles          <- generateModule.usedFunctions
                                  .filter(sffqn => !typeGeneratedFunctions.contains(sffqn.value.functionName))
                                  .flatTraverse(sourcedFfqn => createModuleMethod(mainClassGenerator, sourcedFfqn))
      mainClass              <- mainClassGenerator.generate().liftToCompilationIO
      _                      <- (debug(s"Generated ${generateModule.moduleName.show}, with type files: ${typeFiles
                                    .map(_.fileName)
                                    .mkString(", ")}, with function files: ${functionFiles.map(_.fileName).mkString(", ")}") >> process
                                  .registerFact(
                                    GeneratedModule(generateModule.moduleName, typeFiles ++ functionFiles ++ Seq(mainClass))
                                  )).liftIfNoErrors
    } yield ()

    program.runCompilation_()
  }

  private def createModuleMethod(mainClassGenerator: ClassGenerator, sourcedFfqn: Sourced[FunctionFQN])(using
      process: CompilationProcess
  ): CompilationIO[Seq[ClassFile]] = {
    implementations.get(sourcedFfqn.value) match {
      case Some(nativeImplementation) =>
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty).liftToCompilationIO
      case None                       =>
        for {
          functionDefinitionMaybe <- process.getFact(TypeCheckedFunction.Key(sourcedFfqn.value)).liftToCompilationIO
          _                       <- functionDefinitionMaybe match
                                       case Some(functionDefinition) => createModuleMethod(mainClassGenerator, functionDefinition)
                                       case None                     => compilerError(sourcedFfqn.as(s"Could not find implementation."))
        } yield Seq.empty
    }
  }

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
                                         compilerError(
                                           sourcedCalledFfqn.as("Could not find resolved function."),
                                           Seq(s"Looking for function: ${calledFfqn.show}")
                                         )
        } yield ()
      case FunctionLiteral(parameter, body)                                    => ???
    }

  private def createData(
      outerClassGenerator: ClassGenerator,
      sourcedTfqn: Sourced[TypeFQN]
  )(using process: CompilationProcess): CompilationIO[Seq[ClassFile]] =
    for {
      // Define the data object
      innerClassWriter    <- outerClassGenerator.createInnerClassGenerator(sourcedTfqn.value.typeName).liftToCompilationIO
      typeDefinitionMaybe <- process.getFact(ResolvedData.Key(sourcedTfqn.value)).liftToCompilationIO
      _                   <- typeDefinitionMaybe match {
                               case Some(typeDefinition) =>
                                 for {
                                   _ <- compilerAbort(sourcedTfqn.as("Type not fully defined."))
                                          .unlessA(typeDefinition.definition.fields.isDefined)
                                   // Define the inner data fields
                                   _ <- typeDefinition.definition.fields.get.traverse_ { argumentDefinition =>
                                          argumentDefinition.typeReference match {
                                            case DirectTypeReference(dataType, genericParameters) =>
                                              innerClassWriter.createField[CompilationIO](argumentDefinition.name.value, dataType.value)
                                            case GenericTypeReference(name, genericParameters)    =>
                                              innerClassWriter.createField[CompilationIO](argumentDefinition.name.value, systemAnyType)
                                          }
                                        }
                                   // Define constructor
                                   _ <-
                                     innerClassWriter
                                       .createMethod[CompilationIO](
                                         "<init>",
                                         typeDefinition.definition.fields.get.map(_.typeReference).map(simpleType) ++ Seq(systemUnitType)
                                       )
                                       .use { methodGenerator =>
                                         for {
                                           // Call super.<init>
                                           _ <- methodGenerator.runNative[CompilationIO] { methodVisitor =>
                                                  methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
                                                  methodVisitor.visitMethodInsn(
                                                    Opcodes.INVOKESPECIAL,
                                                    "java/lang/Object",
                                                    "<init>",
                                                    "()V",
                                                    false
                                                  )
                                                }
                                           // Set all this.field = field
                                           _ <- typeDefinition.definition.fields.get.zipWithIndex.traverse_ { (fieldDefinition, index) =>
                                                  methodGenerator.runNative[CompilationIO] { methodVisitor =>
                                                    methodVisitor.visitVarInsn(Opcodes.ALOAD, 0) // this
                                                    methodVisitor.visitVarInsn(
                                                      Opcodes.ALOAD,
                                                      index + 1
                                                    )                                            // TODO: doesn't support primitives
                                                    methodVisitor.visitFieldInsn(
                                                      Opcodes.PUTFIELD,
                                                      outerClassGenerator.name.name + "$" + sourcedTfqn.value.typeName,
                                                      fieldDefinition.name.value,
                                                      javaSignatureName(simpleType(fieldDefinition.typeReference))
                                                    )
                                                  }
                                                }

                                         } yield ()
                                       }
                                   // Define data function
                                   _ <-
                                     outerClassGenerator
                                       .createMethod[CompilationIO](
                                         sourcedTfqn.value.typeName, // TODO: is name legal?
                                         typeDefinition.definition.fields.get.map(_.typeReference).map(simpleType) ++ Seq(
                                           sourcedTfqn.value
                                         )
                                       )
                                       .use { methodGenerator =>
                                         for {
                                           // Allocate new data object
                                           _ <- methodGenerator.runNative[CompilationIO] { methodVisitor =>
                                                  methodVisitor.visitTypeInsn(
                                                    Opcodes.NEW,
                                                    outerClassGenerator.name.name + "$" + sourcedTfqn.value.typeName
                                                  )
                                                  methodVisitor.visitInsn(Opcodes.DUP)
                                                }
                                           // Load constructor arguments
                                           _ <- typeDefinition.definition.fields.zipWithIndex.traverse_ { (fieldDefinition, index) =>
                                                  methodGenerator.runNative[CompilationIO] { methodVisitor =>
                                                    methodVisitor.visitVarInsn(Opcodes.ALOAD, index) // TODO: Fix type
                                                  }
                                                }
                                           // Call constructor
                                           _ <- methodGenerator.runNative[CompilationIO] { methodVisitor =>
                                                  methodVisitor.visitMethodInsn(
                                                    Opcodes.INVOKESPECIAL,
                                                    outerClassGenerator.name.name + "$" + sourcedTfqn.value.typeName,
                                                    "<init>",
                                                    calculateSignatureString(
                                                      typeDefinition.definition.fields.get.map(_.typeReference).map(simpleType) ++ Seq(
                                                        systemUnitType
                                                      )
                                                    ),
                                                    false
                                                  )
                                                }
                                         } yield ()
                                       }
                                   // Define accessors
                                   _ <- typeDefinition.definition.fields.get.traverse_ { argumentDefinition =>
                                          outerClassGenerator
                                            .createMethod[CompilationIO](
                                              argumentDefinition.name.value,
                                              Seq(sourcedTfqn.value, simpleType(argumentDefinition.typeReference))
                                            )
                                            .use { accessorGenerator =>
                                              accessorGenerator.runNative[CompilationIO] { methodVisitor =>
                                                methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
                                                methodVisitor.visitFieldInsn(
                                                  Opcodes.GETFIELD,
                                                  outerClassGenerator.name.name + "$" + sourcedTfqn.value.typeName,
                                                  argumentDefinition.name.value,
                                                  javaSignatureName(simpleType(argumentDefinition.typeReference))
                                                )
                                              }
                                            }
                                        }
                                 } yield ()
                               case None                 =>
                                 compilerError(sourcedTfqn.as("Could not find resolved type."))
                             }
      classFile           <- innerClassWriter.generate().liftToCompilationIO
    } yield Seq(classFile)

  private def simpleType(typeReference: TypeReference): TypeFQN =
    typeReference match {
      case DirectTypeReference(dataType, genericParameters) => dataType.value
      case GenericTypeReference(name, genericParameters)    => systemAnyType
    }

  private def collectTypeGeneratedFunctions(sourcedTfqn: Sourced[TypeFQN])(using
      process: CompilationProcess
  ): CompilationIO[Seq[String]] =
    process.getFact(ResolvedData.Key(sourcedTfqn.value)).liftToCompilationIO.map {
      case Some(resolvedData) =>
        resolvedData.definition.fields.getOrElse(Seq.empty).map(_.name.value) ++ Seq(sourcedTfqn.value.typeName)
      case None               => Seq(sourcedTfqn.value.typeName)
    }
}
