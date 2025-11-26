package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.NativeType.javaSignatureName
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType, systemUnitType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

import scala.annotation.tailrec

class JvmClassGenerator extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case GenerateModule(moduleName, usedFunctions, usedTypes) => generateClass(moduleName, usedFunctions, usedTypes)
      case _                                                   => IO.unit

  private def generateClass(
      moduleName: ModuleName,
      usedFunctions: Seq[Sourced[FunctionFQN]],
      usedTypes: Seq[Sourced[TypeFQN]]
  )(using
      process: CompilationProcess
  ): IO[Unit] = {
    val classWriter = createClassWriter(moduleName)

    (for {
      _ <- usedTypes.traverse_(sourcedTfqn => createType(classWriter, sourcedTfqn))
      _ <- usedFunctions.traverse_(sourcedFfqn => createClassMethod(classWriter, sourcedFfqn))
      _ <- IO(classWriter.visitEnd()).liftToCompilationIO
      _ <- process
             .registerFact(GeneratedModule(moduleName, classWriter.toByteArray))
             .liftIfNoErrors
    } yield ()).runCompilation_()
  }

  private def createClassMethod(classWriter: ClassWriter, sourcedFfqn: Sourced[FunctionFQN])(using
      process: CompilationProcess
  ): CompilationIO[Unit] =
    for {
      functionDefinitionMaybe <- process.getFact(TypeCheckedFunction.Key(sourcedFfqn.value)).liftToCompilationIO
      _                       <- functionDefinitionMaybe match {
                                   case Some(functionDefinition) => createClassMethod(classWriter, functionDefinition).liftToCompilationIO
                                   case None                     =>
                                     implementations.get(sourcedFfqn.value) match {
                                       case Some(nativeImplementation) => nativeImplementation.generateMethod(classWriter).liftToCompilationIO
                                       case None                       => compilerError(sourcedFfqn.as(s"Could not find implementation."))
                                     }
                                 }
    } yield ()

  private def calculateSignatureString(signatureTypes: Seq[TypeFQN]): String =
    s"(${signatureTypes.init.map(javaSignatureName).mkString})${javaSignatureName(signatureTypes.last)}"

  private def createClassMethod(classWriter: ClassWriter, functionDefinition: TypeCheckedFunction)(using
      CompilationProcess
  ): IO[Unit] =
    val signatureTypes = calculateMethodSignature(functionDefinition.definition.valueType)

    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      functionDefinition.ffqn.functionName, // TODO: can every method name be converted to Java?
      calculateSignatureString(signatureTypes),
      null,
      null
    )

    for {
      _   <- IO(methodVisitor.visitCode())
      body = extractMethodBody(functionDefinition.definition.body.get.value, signatureTypes.length)
      _   <- createExpressionCode(methodVisitor, body)
      _   <- IO {
               if (signatureTypes.last === systemUnitType) {
                 methodVisitor.visitInsn(Opcodes.RETURN)
               } else {
                 methodVisitor.visitInsn(Opcodes.ARETURN)
               }
               methodVisitor.visitMaxs(0, 0)
               methodVisitor.visitEnd()
             }
    } yield ()

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

  private def createExpressionCode(methodVisitor: MethodVisitor, expression: Expression)(using
      CompilationProcess
  ): IO[Unit] =
    expression match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        generateFunctionApplication(methodVisitor, target, Seq(argument)) // One-argument call
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        =>
        IO(methodVisitor.visitLdcInsn(stringLiteral.value))
      case ParameterReference(parameterName)                                   => ???
      case ValueReference(Sourced(_, _, ffqn))                                 =>
        // No-argument call
        generateFunctionApplication(methodVisitor, expression, Seq.empty)
      case FunctionLiteral(parameter, body)                                    => ???
    }

  @tailrec
  private def generateFunctionApplication(
      methodVisitor: MethodVisitor,
      target: Expression,
      arguments: Seq[Expression]
  )(using process: CompilationProcess): IO[Unit] =
    target match {
      case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        // Means this is another argument, so just recurse
        generateFunctionApplication(methodVisitor, target, arguments.appended(argument))
      case IntegerLiteral(integerLiteral)                                      => ???
      case StringLiteral(stringLiteral)                                        => ???
      case ParameterReference(parameterName)                                   => ???
      case ValueReference(sourcedCalledFfqn @ Sourced(_, _, calledFfqn))       =>
        // Calling a function with exactly one argument
        for {
          functionDefinitionMaybe <- process.getFact(ResolvedFunction.Key(calledFfqn))
          _                       <- functionDefinitionMaybe match
                                       case Some(functionDefinition) =>
                                         for {
                                           _ <- arguments.traverse_(expression => createExpressionCode(methodVisitor, expression))
                                           _ <- IO(
                                                  methodVisitor.visitMethodInsn(
                                                    Opcodes.INVOKESTATIC,
                                                    calledFfqn.moduleName.packages
                                                      .appended(calledFfqn.moduleName.name)
                                                      .mkString("/"),        // TODO: export this
                                                    calledFfqn.functionName, // TODO: not a safe name
                                                    calculateSignatureString(
                                                      calculateMethodSignature(functionDefinition.definition.valueType)
                                                    ),
                                                    false
                                                  )
                                                )
                                         } yield ()
                                       case None                     =>
                                         registerCompilerError(sourcedCalledFfqn.as(s"Could not find type checked ${calledFfqn.show}"))
        } yield ()
      case FunctionLiteral(parameter, body)                                    => ???
    }

  private def createClassWriter(name: ModuleName): ClassWriter = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
      name.name, // TODO: all class names are legal here?
      null,
      "java/lang/Object",
      null
    )

    classWriter
  }

  private def createType(writer: ClassWriter, value: Sourced[TypeFQN]): CompilationIO[Unit] =
    for {
      // Define the data object
      _ <- IO {
             ???
           }.liftToCompilationIO
    } yield ()
}
