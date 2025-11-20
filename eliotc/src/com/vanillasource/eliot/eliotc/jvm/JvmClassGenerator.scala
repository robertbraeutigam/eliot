package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import org.objectweb.asm.{ClassWriter, Opcodes}

class JvmClassGenerator extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case GenerateClass(moduleName, usedFunctions) => generateClass(moduleName, usedFunctions)
      case _                                        => IO.unit

  def generateClass(moduleName: ModuleName, usedFunctions: Seq[(FunctionFQN, Sourced[_])])(using
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

  private def createClassMethod(classWriter: ClassWriter, functionDefinition: TypeCheckedFunction): Unit = {}

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
