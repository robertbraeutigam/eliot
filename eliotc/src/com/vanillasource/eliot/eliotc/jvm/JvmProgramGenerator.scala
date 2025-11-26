package com.vanillasource.eliot.eliotc.jvm

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.used.UsedSymbols
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import org.objectweb.asm.{ClassWriter, Opcodes}

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(mainFunction: FunctionFQN, targetDir: Path) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case UsedSymbols(usedFunctions, usedTypes) => generateAllClasses(usedFunctions, usedTypes)
      case _                                     => IO.unit

  private def generateAllClasses(
      usedFunctions: Seq[Sourced[FunctionFQN]],
      usedTypes: Seq[Sourced[TypeFQN]]
  )(using process: CompilationProcess): IO[Unit] = {
    val groupedFunctions = usedFunctions.groupBy(_.value.moduleName)
    val groupedTypes     = usedTypes.groupBy(_.value.moduleName)
    val facts            = (groupedFunctions.keys ++ groupedTypes.keys)
      .map(moduleName =>
        GenerateModule(
          moduleName,
          groupedFunctions.getOrElse(moduleName, Seq.empty),
          groupedTypes.getOrElse(moduleName, Seq.empty)
        )
      )
      .toSeq

    for {
      _            <- facts.traverse_(process.registerFact)
      classesMaybe <- facts.map(_.moduleName).traverse(moduleName => process.getFact(GeneratedModule.Key(moduleName)))
      _            <- classesMaybe.sequence.traverse_(generateJarFile) // Skips jar if not all modules got bytecode
    } yield ()
  }

  private def generateJarFile(allClasses: Seq[GeneratedModule]): IO[Unit] =
    jarOutputStream.use { jos =>
      IO.blocking {
        generateManifest(jos)
        generateClasses(jos, allClasses)
        generateMain(jos)
      }
    }

  private def generateClasses(jos: JarOutputStream, allClasses: Seq[GeneratedModule]): Unit = {
    allClasses.foreach { case GeneratedModule(moduleName, bytes) =>
      val pathName  = if (moduleName.packages.isEmpty) "" else moduleName.packages.mkString("", "/", "/")
      val entryName = moduleName.name + ".class" // FIXME: same javaname conversion as in class! Use the class name!
      val entry     = new JarEntry(pathName + entryName)

      jos.putNextEntry(entry)
      jos.write(bytes)
      jos.closeEntry()
    }
  }

  private def jarOutputStream: Resource[IO, JarOutputStream] =
    for {
      _   <- Resource.eval(IO.blocking(Files.createDirectories(targetDir)))
      os  <- Resource.fromAutoCloseable(IO.blocking(Files.newOutputStream(jarFilePath, CREATE, TRUNCATE_EXISTING)))
      jos <- Resource.fromAutoCloseable(IO.blocking(new JarOutputStream(os)))
    } yield jos

  private def jarFilePath: Path = targetDir.resolve(jarFileName)

  private def jarFileName: String = mainFunction.moduleName.name + ".jar"

  private def generateMain(jos: JarOutputStream): Unit = {
    jos.putNextEntry(new JarEntry("main.class"))
    jos.write(generateMainClassBytes)
    jos.closeEntry()
  }

  private def generateMainClassBytes: Array[Byte] = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
      "main",
      null,
      "java/lang/Object",
      null
    )

    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      "main",
      "([Ljava/lang/String;)V",
      null,
      null
    )

    methodVisitor.visitCode()

    methodVisitor.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      mainFunction.moduleName.packages.appended(mainFunction.moduleName.name).mkString("/"),
      "main",
      "()V",
      false
    )

    methodVisitor.visitInsn(Opcodes.RETURN)
    methodVisitor.visitMaxs(0, 0)
    methodVisitor.visitEnd()

    classWriter.visitEnd()

    classWriter.toByteArray
  }

  private def generateManifest(jos: JarOutputStream): Unit = {
    jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
    jos.write("Manifest-Version: 1.0\nMain-Class: main\n".getBytes(StandardCharsets.UTF_8))
    jos.closeEntry()
  }
}
