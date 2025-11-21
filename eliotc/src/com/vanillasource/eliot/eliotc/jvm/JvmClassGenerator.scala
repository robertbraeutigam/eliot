package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.NativeType.{javaSignatureName, types}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

import scala.annotation.tailrec

class JvmClassGenerator extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case GenerateClass(moduleName, usedFunctions) => generateClass(moduleName, usedFunctions)
      case _                                        => IO.unit

  private def generateClass(moduleName: ModuleName, usedFunctions: Seq[(FunctionFQN, Sourced[_])])(using
      process: CompilationProcess
  ): IO[Unit] = {
    val classWriter = createClassWriter(moduleName)

    (for {
      _ <- usedFunctions.traverse_(tuple => createClassMethod(classWriter, tuple._1, tuple._2))
      _ <- IO(classWriter.visitEnd()).liftToCompilationIO
      _ <- process
             .registerFact(GeneratedClass(moduleName, classWriter.toByteArray))
             .liftIfNoErrors
    } yield ()).runCompilation_()
  }

  private def createClassMethod(classWriter: ClassWriter, ffqn: FunctionFQN, exampleUsage: Sourced[_])(using
      process: CompilationProcess
  ): CompilationIO[Unit] =
    for {
      functionDefinitionMaybe <- process.getFact(TypeCheckedFunction.Key(ffqn)).liftToCompilationIO
      _                       <- functionDefinitionMaybe match {
                                   case Some(functionDefinition) => IO(createClassMethod(classWriter, functionDefinition)).liftToCompilationIO
                                   case None                     => compilerError(exampleUsage.as(s"Could not find implementation."))
                                 }
    } yield ()

  private def createClassMethod(classWriter: ClassWriter, functionDefinition: TypeCheckedFunction): Unit = {
    val signatureTypes = calculateMethodSignature(functionDefinition.definition.valueType)

    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      functionDefinition.ffqn.functionName, // FIXME: can every method name be converted to Java?
      s"(${signatureTypes.init.map(javaSignatureName).mkString})${javaSignatureName(signatureTypes.last)}",
      null,
      null
    )

    methodVisitor.visitCode()

    val body = extractMethodBody(functionDefinition.definition.body.get.value, signatureTypes.length)
    createExpressionCode(methodVisitor, body)

    methodVisitor.visitInsn(Opcodes.RETURN)
    methodVisitor.visitMaxs(0, 0)
    methodVisitor.visitEnd()
  }

  /** Extracts parameter arity from curried form.
    */
  private def calculateMethodSignature(typeReference: TypeReference): Seq[TypeFQN] =
    typeReference match {
      case DirectTypeReference(Sourced(_, _, dataType), genericParameters) if dataType === systemFunctionType =>
        calculateMethodSignature(genericParameters(0)) ++ calculateMethodSignature(genericParameters(1))
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

  private def createExpressionCode(methodVisitor: MethodVisitor, expression: Expression): Unit =
    expression match {
      case FunctionApplication(target @ Sourced(_, _, ValueReference(Sourced(_, _, calledFfqn))), argument)
          if implementations.contains(calledFfqn) =>
        // Called an native
        implementations(calledFfqn)
          .withArguments(methodVisitor, createExpressionCode(methodVisitor, argument.value))
      case FunctionApplication(target, argument) => ???
      case IntegerLiteral(integerLiteral)        => ???
      case StringLiteral(stringLiteral)          => methodVisitor.visitLdcInsn(stringLiteral.value)
      case ParameterReference(parameterName)     => ???
      case ValueReference(Sourced(_, _, ffqn))   => ??? // generateFunctionCall(methodVisitor, ffqn)
      case FunctionLiteral(parameter, body)      => ???
    }

  /*
  private def generateFunctionCall(methodVisitor: MethodVisitor, ffqn: FunctionFQN): Unit = {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      "owner/internal/Name",
      "methodName",
      "(ArgumentDescriptor...)ReturnDescriptor",
      false
    )
  }*/

  private def createClassWriter(name: ModuleName): ClassWriter = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC,
      name.name, // FIXME: all class names are legal here?
      null,
      "java/lang/Object",
      null
    )

    classWriter
  }
}
