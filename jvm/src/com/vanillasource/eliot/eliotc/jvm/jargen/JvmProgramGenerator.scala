package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.dynamic.DynamicContent.addDynamicSource
import com.vanillasource.eliot.eliotc.used.UsedNames

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class JvmProgramGenerator(targetDir: Path, sourceDir: Path)
    extends SingleFactProcessor[GenerateExecutableJar.Key]
    with Logging {

  override protected def generateSingleFact(key: GenerateExecutableJar.Key): CompilerIO[GenerateExecutableJar] =
    for {
      // First, generate all modules for user's code
      _          <- generateModulesFrom(key.vfqn)
      // Add dynamic main and generate everything if user's code did not fail
      _          <- addDynamicSource(Path.of("main.els"), generateMainSource(key.vfqn))
      allModules <- generateModulesFrom(ValueFQN(ModuleName(Seq(), "main"), "main"))
      _          <- generateJarFile(key.vfqn, allModules).to[CompilerIO]
    } yield GenerateExecutableJar(key.vfqn)

  private def generateModulesFrom(vfqn: ValueFQN): CompilerIO[Seq[GeneratedModule]] =
    for {
      usedNames <- getFactOrAbort(UsedNames.Key(vfqn))
      modules   <- generateModules(vfqn, usedNames)
    } yield modules

  private def generateModules(vfqn: ValueFQN, usedNames: UsedNames): CompilerIO[Seq[GeneratedModule]] = {
    val moduleNames = usedNames.usedNames.keys.map(_.moduleName).toSeq.distinct

    moduleNames.traverse(moduleName => getFactOrAbort(GeneratedModule.Key(moduleName, vfqn)))
  }

  private def generateJarFile(mainValue: ValueFQN, allClasses: Seq[GeneratedModule]): IO[Unit] =
    jarOutputStream(mainValue).use { jos =>
      IO.blocking {
        generateManifest(jos)
        generateClasses(jos, allClasses)
      }
    } >> info(s"Generated executable jar: ${jarFilePath(mainValue)}.")

  private def generateClasses(jos: JarOutputStream, allClasses: Seq[GeneratedModule]): Unit = {
    allClasses.foreach { case GeneratedModule(moduleName, vfqn, classFiles) =>
      classFiles.foreach { classFile =>
        jos.putNextEntry(new JarEntry(classFile.fileName))
        jos.write(classFile.bytecode)
        jos.closeEntry()
      }
    }
  }

  private def jarOutputStream(mainValue: ValueFQN): Resource[IO, JarOutputStream] =
    for {
      _   <- Resource.eval(IO.blocking(Files.createDirectories(targetDir)))
      os  <- Resource.fromAutoCloseable(
               IO.blocking(Files.newOutputStream(jarFilePath(mainValue), CREATE, TRUNCATE_EXISTING))
             )
      jos <- Resource.fromAutoCloseable(IO.blocking(new JarOutputStream(os)))
    } yield jos

  private def jarFilePath(mainValue: ValueFQN): Path = targetDir.resolve(jarFileName(mainValue))

  private def jarFileName(mainValue: ValueFQN): String = mainValue.moduleName.name + ".jar"

  private def generateManifest(jos: JarOutputStream): Unit = {
    jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
    jos.write("Manifest-Version: 1.0\nMain-Class: main\n".getBytes(StandardCharsets.UTF_8))
    jos.closeEntry()
  }

  private def generateMainSource(mainVfqn: ValueFQN): String =
    s"""
       |main: Unit = apply(block(${mainVfqn.moduleName.show}::${mainVfqn.name}), unit)
       |""".stripMargin
}
