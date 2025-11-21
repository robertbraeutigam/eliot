package com.vanillasource.eliot.eliotc.jvm

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.NativeImplementations.implementations
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.used.UsedSymbols

import java.nio.file.StandardOpenOption.CREATE_NEW
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(mainFunction: FunctionFQN, targetDir: Path) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case UsedSymbols(usedFunctions) => generateAllClasses(usedFunctions)
      case _                          => IO.unit

  private def generateAllClasses(
      usedFunctions: Map[FunctionFQN, Sourced[_]]
  )(using process: CompilationProcess): IO[Unit] = {
    // Remove native functions from used functions
    val groupedFunctions = usedFunctions.filter(uf => !implementations.contains(uf._1)).toSeq.groupBy(_._1.moduleName)

    for {
      _            <- groupedFunctions.toSeq
                        .traverse_((moduleName, usedFunctions) => process.registerFact(GenerateClass(moduleName, usedFunctions)))
      classesMaybe <-
        groupedFunctions.keys.toSeq.traverse(moduleName => process.getFact(GeneratedClass.Key(moduleName)))
      _            <- classesMaybe.sequence.traverse_(generateJarFile) // Skips jar if not all modules got bytecode
    } yield ()
  }

  private def generateJarFile(allClasses: Seq[GeneratedClass]): IO[Unit] =
    jarOutputStream.use { jos =>
      IO.blocking {
        allClasses.foreach { case GeneratedClass(moduleName, bytes) =>
          val pathName  = if (moduleName.packages.isEmpty) "" else moduleName.packages.mkString("", "/", "/")
          val entryName = moduleName.name + ".class" // FIXME: same javaname conversion as in class! Use the class name!
          val entry     = new JarEntry(pathName + entryName)

          jos.putNextEntry(entry)
          jos.write(bytes)
          jos.closeEntry()
        }
      }
    }

  private def jarOutputStream: Resource[IO, JarOutputStream] =
    for {
      _   <- Resource.eval(IO.blocking(Files.createDirectories(targetDir)))
      os  <- Resource.fromAutoCloseable(IO.blocking(Files.newOutputStream(jarFilePath, CREATE_NEW)))
      jos <- Resource.fromAutoCloseable(IO.blocking(new JarOutputStream(os)))
    } yield jos

  private def jarFilePath: Path = targetDir.resolve(jarFileName)

  private def jarFileName: String = mainFunction.moduleName.name + ".jar"
}
