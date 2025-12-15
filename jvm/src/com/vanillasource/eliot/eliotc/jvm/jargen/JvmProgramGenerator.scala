package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.NativeType.javaSignatureName
import com.vanillasource.eliot.eliotc.jvm.classgen.{GenerateModule, GeneratedModule}
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemIOType
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.used.UsedSymbols
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact}
import org.objectweb.asm.{ClassWriter, Opcodes}

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(targetDir: Path)
    extends OneToOneProcessor((key: GenerateExecutableJar.Key) => UsedSymbols.Key(key.ffqn))
    with Logging {

  override def generateFromFact(usedSymbols: UsedSymbols)(using CompilationProcess): IO[Unit] = {
    val groupedFunctions = usedSymbols.usedFunctions.groupBy(_.value.moduleName)
    val groupedTypes     = usedSymbols.usedTypes.groupBy(_.value.moduleName)
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
      _            <- facts.traverse_(registerFact)
      classesMaybe <- facts.map(_.moduleName).traverse(moduleName => getFact(GeneratedModule.Key(moduleName)))
      _            <- classesMaybe.sequence.traverse_(cs =>
                        generateJarFile(usedSymbols.ffqn, cs)
                      ) // Skips jar if not all modules got bytecode
    } yield ()
  }

  private def generateJarFile(mainFunction: FunctionFQN, allClasses: Seq[GeneratedModule]): IO[Unit] =
    jarOutputStream(mainFunction).use { jos =>
      IO.blocking {
        generateManifest(jos)
        generateClasses(jos, allClasses)
        generateMain(mainFunction, jos)
      }
    } >> info(s"Generated executable jar: ${jarFilePath(mainFunction)}.")

  private def generateClasses(jos: JarOutputStream, allClasses: Seq[GeneratedModule]): Unit = {
    allClasses.foreach { case GeneratedModule(moduleName, classFiles) =>
      classFiles.foreach { classFile =>
        jos.putNextEntry(new JarEntry(classFile.fileName))
        jos.write(classFile.bytecode)
        jos.closeEntry()
      }
    }
  }

  private def jarOutputStream(mainFunction: FunctionFQN): Resource[IO, JarOutputStream] =
    for {
      _   <- Resource.eval(IO.blocking(Files.createDirectories(targetDir)))
      os  <- Resource.fromAutoCloseable(
               IO.blocking(Files.newOutputStream(jarFilePath(mainFunction), CREATE, TRUNCATE_EXISTING))
             )
      jos <- Resource.fromAutoCloseable(IO.blocking(new JarOutputStream(os)))
    } yield jos

  private def jarFilePath(mainFunction: FunctionFQN): Path = targetDir.resolve(jarFileName(mainFunction))

  private def jarFileName(mainFunction: FunctionFQN): String = mainFunction.moduleName.name + ".jar"

  private def generateMain(mainFunction: FunctionFQN, jos: JarOutputStream): Unit = {
    jos.putNextEntry(new JarEntry("main.class"))
    jos.write(generateMainClassBytes(mainFunction))
    jos.closeEntry()
  }

  private def generateMainClassBytes(mainFunction: FunctionFQN): Array[Byte] = {
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
      s"()${javaSignatureName(systemIOType)}",
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
