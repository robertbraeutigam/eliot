package com.vanillasource.eliot.eliotc.jvm

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

trait EliotIntegrationTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  protected def compileAndRun(source: String, moduleName: String = "Test"): IO[String] =
    (for {
      sourceDir <- tempDirectory("eliot-src")
      targetDir <- tempDirectory("eliot-target")
    } yield (sourceDir, targetDir)).use { (sourceDir, targetDir) =>
      for {
        _      <- IO.blocking(Files.writeString(sourceDir.resolve(s"$moduleName.els"), source))
        _      <- Compiler.runCompiler(
                    List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", moduleName)
                  )
        output <- runJar(targetDir.resolve(s"$moduleName.jar"))
      } yield output
    }

  private def runJar(jarPath: Path): IO[String] = IO.blocking {
    val classLoader = new URLClassLoader(Array(jarPath.toUri.toURL), ClassLoader.getPlatformClassLoader)
    try {
      val mainClass  = classLoader.loadClass("main")
      val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
      val baos       = new ByteArrayOutputStream()
      val printStream = new PrintStream(baos)
      val oldOut     = System.out
      try {
        System.setOut(printStream)
        Console.withOut(printStream) {
          mainMethod.invoke(null, Array.empty[String])
        }
      } finally {
        System.setOut(oldOut)
      }
      printStream.flush()
      baos.toString.stripLineEnd
    } finally {
      classLoader.close()
    }
  }

  private def tempDirectory(prefix: String): Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory(prefix)))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
