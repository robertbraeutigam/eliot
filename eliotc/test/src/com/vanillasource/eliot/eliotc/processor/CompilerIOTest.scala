package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class CompilerIOTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "context" should "be clean if nothing yet happened" in {
    runCompilerIO {
      isClear
    }.asserting(_ shouldBe Some(true))
  }

  private def runCompilerIO[T](value: CompilerIO[T]): IO[Option[T]] =
    value.run(null).run.fold(_ => None, t => Some(t._2))
}
