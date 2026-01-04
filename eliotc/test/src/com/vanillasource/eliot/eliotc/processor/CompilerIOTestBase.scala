package com.vanillasource.eliot.eliotc.processor

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** Base class for tests that use CompilerIO and need test fixtures for facts and compilation process.
  */
abstract class CompilerIOTestBase extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  /** Helper method to run a CompilerIO computation with a test compilation process.
    *
    * @param process
    *   The compilation process to use, or null to use no process
    * @param value
    *   The CompilerIO computation to run
    * @return
    *   An IO containing either errors or the result
    */
  protected def runCompilerIO[T](
      process: CompilationProcess = null
  )(value: CompilerIO[T]): IO[Either[Chain[CompilerError], T]] =
    value.run(process).run(Chain.empty).value.map {
      case Left(errors)  => Left(errors)
      case Right((_, t)) => Right(t)
    }
}

object CompilerIOTestBase {

  /** Test fact implementation for testing purposes.
    */
  case class TestFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[TestFact] = TestFactKey(value)
  }

  /** Test fact key for TestFact.
    */
  case class TestFactKey(value: String) extends CompilerFactKey[TestFact]

  /** Test implementation of CompilationProcess that stores facts in memory.
    */
  class TestCompilationProcess extends CompilationProcess {
    var facts: Map[CompilerFactKey[?], CompilerFact] = Map.empty

    def registerFactSync(fact: CompilerFact): Unit = {
      facts = facts.updated(fact.key(), fact)
    }

    override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
      IO.pure(facts.get(key).map(_.asInstanceOf[V]))

    override def registerFact(value: CompilerFact): IO[Unit] =
      IO {
        registerFactSync(value)
      }
  }
}
