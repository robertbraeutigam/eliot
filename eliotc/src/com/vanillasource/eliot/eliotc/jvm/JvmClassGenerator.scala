package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.NativeImplementations.implementations
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

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
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      functionDefinition.ffqn.functionName, // FIXME: can every method name be converted to Java?
      "([Ljava/lang/String;)V",             // FIXME: this is obviously wrong
      null,
      null
    )

    methodVisitor.visitCode()

    createExpressionCode(methodVisitor, functionDefinition.definition.body.get.value)

    methodVisitor.visitInsn(Opcodes.RETURN)
    methodVisitor.visitMaxs(0, 0)
    methodVisitor.visitEnd()
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
      case ValueReference(valueName)             => ???
      case FunctionLiteral(parameter, body)      => ???
    }

  private def createClassWriter(name: ModuleName): ClassWriter = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC,
      "name",
      null,
      "java/lang/Object",
      null
    )

    classWriter
  }
}
