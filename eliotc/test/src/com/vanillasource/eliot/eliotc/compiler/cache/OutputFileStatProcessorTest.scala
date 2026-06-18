package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.getFactOrAbort
import com.vanillasource.eliot.eliotc.processor.ProcessorTest

import java.io.File
import java.nio.file.Files

class OutputFileStatProcessorTest extends ProcessorTest {
  private val processor = new OutputFileStatProcessor()

  "OutputFileStatProcessor" should "report a present file" in {
    withTempFile { file =>
      runCompilerIO {
        processor.generate(OutputFileStat.Key(file)) >> getFactOrAbort(OutputFileStat.Key(file))
      }.asserting(_ shouldBe Right(OutputFileStat(file, present = true)))
    }
  }

  it should "report a missing file" in {
    withTempFile { file =>
      IO.blocking(file.delete()) >> runCompilerIO {
        processor.generate(OutputFileStat.Key(file)) >> getFactOrAbort(OutputFileStat.Key(file))
      }.asserting(_ shouldBe Right(OutputFileStat(file, present = false)))
    }
  }

  private def withTempFile[A](use: File => IO[A]): IO[A] =
    IO.blocking(Files.createTempFile("eliot-output-stat", ".jar").toFile)
      .bracket(use)(file => IO.blocking(file.delete()).void)
}
