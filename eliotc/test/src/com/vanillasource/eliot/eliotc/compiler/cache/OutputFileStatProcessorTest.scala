package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.getFactOrAbort
import com.vanillasource.eliot.eliotc.processor.ProcessorTest

import java.io.File
import java.nio.file.Files

class OutputFileStatProcessorTest extends ProcessorTest {
  private val processor = new OutputFileStatProcessor()

  // The SHA-256 of the empty input, i.e. of a file that exists but has no content.
  private val emptyDigest = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  "OutputFileStatProcessor" should "digest a present file" in {
    withTempFile { file =>
      runCompilerIO {
        processor.generate(OutputFileStat.Key(file)) >> getFactOrAbort(OutputFileStat.Key(file))
      }.asserting(_ shouldBe Right(OutputFileStat(file, Some(emptyDigest))))
    }
  }

  it should "digest a file's content, not its name" in {
    withTempFile { file =>
      IO.blocking(Files.writeString(file.toPath, "content")) >> runCompilerIO {
        processor.generate(OutputFileStat.Key(file)) >> getFactOrAbort(OutputFileStat.Key(file))
      }.asserting(_ should not be Right(OutputFileStat(file, Some(emptyDigest))))
    }
  }

  it should "report a missing file" in {
    withTempFile { file =>
      IO.blocking(file.delete()) >> runCompilerIO {
        processor.generate(OutputFileStat.Key(file)) >> getFactOrAbort(OutputFileStat.Key(file))
      }.asserting(_ shouldBe Right(OutputFileStat(file, None)))
    }
  }

  private def withTempFile[A](use: File => IO[A]): IO[A] =
    IO.blocking(Files.createTempFile("eliot-output-stat", ".jar").toFile)
      .bracket(use)(file => IO.blocking(file.delete()).void)
}
