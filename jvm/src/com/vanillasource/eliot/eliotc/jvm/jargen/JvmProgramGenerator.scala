package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.SourceContent.addSource
import com.vanillasource.eliot.eliotc.used.UsedSymbols

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(targetDir: Path, sourceDir: Path)
    extends SingleKeyTypeProcessor[GenerateExecutableJar.Key]
    with Logging {

  override protected def generateFact(key: GenerateExecutableJar.Key): CompilerIO[Unit] =
    for {
      // First, generate all modules for user's code
      _          <- generateModulesFrom(key.ffqn)
      // Add dynamic main and generate everything if user's code did not fail
      _          <- addSource(sourceDir.resolve("main.els").toFile, generateMainSource(key.ffqn))
      allModules <- generateModulesFrom(FunctionFQN(ModuleName(Seq(), "main"), "main"))
      _          <- generateJarFile(key.ffqn, allModules).to[CompilerIO]
    } yield ()

  private def generateModulesFrom(ffqn: FunctionFQN): CompilerIO[Seq[GeneratedModule]] =
    for {
      usedSymbols <- getFactOrAbort(UsedSymbols.Key(ffqn))
      modules     <- generateModules(ffqn, usedSymbols)
    } yield modules

  private def generateModules(ffqn: FunctionFQN, usedSymbols: UsedSymbols): CompilerIO[Seq[GeneratedModule]] = {
    val groupedFunctions = usedSymbols.usedFunctions.groupBy(_.value.moduleName)
    val groupedTypes     = usedSymbols.usedTypes.groupBy(_.value.moduleName)
    val moduleNames      = (groupedFunctions.keys ++ groupedTypes.keys).toSeq

    moduleNames.traverse(moduleName => getFactOrAbort(GeneratedModule.Key(moduleName, ffqn)))
  }

  private def generateJarFile(mainFunction: FunctionFQN, allClasses: Seq[GeneratedModule]): IO[Unit] =
    jarOutputStream(mainFunction).use { jos =>
      IO.blocking {
        generateManifest(jos)
        generateClasses(jos, allClasses)
      }
    } >> info(s"Generated executable jar: ${jarFilePath(mainFunction)}.")

  private def generateClasses(jos: JarOutputStream, allClasses: Seq[GeneratedModule]): Unit = {
    allClasses.foreach { case GeneratedModule(moduleName, ffqn, classFiles) =>
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
       |main: Unit = apply(block(${mainFfqn.moduleName.show}::${mainFfqn.functionName}), unit)
       |""".stripMargin
}
