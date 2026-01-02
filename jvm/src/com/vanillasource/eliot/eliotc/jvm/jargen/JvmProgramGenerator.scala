package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.Monad
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.{GenerateModule, GeneratedModule}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.SourceContent.addSource
import com.vanillasource.eliot.eliotc.used.UsedSymbols
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(targetDir: Path, sourceDir: Path) extends CompilerProcessor with Logging {

  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case GenerateExecutableJar.Key(ffqn) =>
        addSource(sourceDir.resolve("main.els").toFile, generateMainSource(ffqn)) >> getFact(
          UsedSymbols.Key(FunctionFQN(ModuleName(Seq(), "main"), "main"))
        )
          .flatMap(_.traverse_(us => generateFromFact(ffqn, us)))
      case _                               => Monad[CompilerIO].unit
    }

  private def generateFromFact(requestedFfqn: FunctionFQN, usedSymbols: UsedSymbols): CompilerIO[Unit] = {
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
      _            <- facts.traverse_(registerFactIfClear) // FIXME: do this in a linear way, not circular way
      classesMaybe <- facts.map(_.moduleName).traverse(moduleName => getFact(GeneratedModule.Key(moduleName)))
      _            <- classesMaybe.sequence
                        .traverse_(cs => generateJarFile(requestedFfqn, cs))
                        .to[CompilerIO] // Skips jar if not all modules got bytecode
    } yield ()
  }

  private def generateJarFile(mainFunction: FunctionFQN, allClasses: Seq[GeneratedModule]): IO[Unit] =
    jarOutputStream(mainFunction).use { jos =>
      IO.blocking {
        generateManifest(jos)
        generateClasses(jos, allClasses)
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

  private def generateManifest(jos: JarOutputStream): Unit = {
    jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
    jos.write("Manifest-Version: 1.0\nMain-Class: main\n".getBytes(StandardCharsets.UTF_8))
    jos.closeEntry()
  }

  private def generateMainSource(mainFfqn: FunctionFQN): String =
    s"""
       |import eliot.java.lang.Array
       |main(args: Array[String]): Unit = apply(block(${mainFfqn.moduleName.show}::${mainFfqn.functionName}), unit)
       |""".stripMargin
}
