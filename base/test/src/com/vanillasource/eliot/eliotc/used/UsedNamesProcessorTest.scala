package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class UsedNamesProcessorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile       = URI.create("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val sourceContent  = SourceContent(testFile, Sourced(testFile, PositionRange.zero, "test source"))

  private val intVfqn = ValueFQN(testModuleName, "Int")
  private val intType = Types.dataType(intVfqn)

  "UsedNamesProcessor" should "include root name in used names for value with no body" in {
    val valueVfqn = ValueFQN(testModuleName, "value")
    val mv        = MonomorphicValue(valueVfqn, Seq.empty, sourced("value"), intType, None)

    runProcessor(UsedNames.Key(valueVfqn), Seq(mv))
      .asserting(_.usedNames should contain key valueVfqn)
  }

  it should "include root name in used names when it references another value" in {
    val fVfqn = ValueFQN(testModuleName, "f")
    val gVfqn = ValueFQN(testModuleName, "g")

    val gMv = MonomorphicValue(gVfqn, Seq.empty, sourced("g"), intType, None)
    val fMv = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(valueRef(gVfqn)))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv, gMv))
      .asserting(_.usedNames should contain key fVfqn)
  }

  it should "include referenced value in used names" in {
    val fVfqn = ValueFQN(testModuleName, "f")
    val gVfqn = ValueFQN(testModuleName, "g")

    val gMv = MonomorphicValue(gVfqn, Seq.empty, sourced("g"), intType, None)
    val fMv = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(valueRef(gVfqn)))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv, gMv))
      .asserting(_.usedNames should contain key gVfqn)
  }

  it should "track direct call application count" in {
    val fVfqn = ValueFQN(testModuleName, "f")
    val gVfqn = ValueFQN(testModuleName, "g")

    val gMv   = MonomorphicValue(gVfqn, Seq.empty, sourced("g"), intType, None)
    val gRef  = MonomorphicExpression(intType, valueRef(gVfqn))
    val arg   = MonomorphicExpression(intType, MonomorphicExpression.IntegerLiteral(sourced(BigInt(42))))
    val app   = MonomorphicExpression.FunctionApplication(sourced(gRef), sourced(arg))
    val fMv   = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(app))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv, gMv))
      .asserting(_.usedNames(gVfqn).directCallApplications shouldBe Map(1 -> 1))
  }

  it should "follow references through function literal bodies" in {
    val fVfqn = ValueFQN(testModuleName, "f")
    val gVfqn = ValueFQN(testModuleName, "g")

    val gMv      = MonomorphicValue(gVfqn, Seq.empty, sourced("g"), intType, None)
    val innerRef = MonomorphicExpression(intType, valueRef(gVfqn))
    val lambda   = MonomorphicExpression.FunctionLiteral(sourced("x"), intType, sourced(innerRef))
    val fMv      = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(lambda))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv, gMv))
      .asserting(_.usedNames should contain key gVfqn)
  }

  it should "handle recursive value without infinite loop" in {
    import scala.concurrent.duration.*

    val fVfqn = ValueFQN(testModuleName, "f")
    val fMv   = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(valueRef(fVfqn)))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv)).timeout(1.seconds)
      .asserting(_.usedNames should contain key fVfqn)
  }

  it should "handle mutual recursion without infinite loop" in {
    import scala.concurrent.duration.*

    val fVfqn = ValueFQN(testModuleName, "f")
    val gVfqn = ValueFQN(testModuleName, "g")

    val fMv = MonomorphicValue(fVfqn, Seq.empty, sourced("f"), intType, runtime(valueRef(gVfqn)))
    val gMv = MonomorphicValue(gVfqn, Seq.empty, sourced("g"), intType, runtime(valueRef(fVfqn)))

    runProcessor(UsedNames.Key(fVfqn), Seq(fMv, gMv)).timeout(1.seconds)
      .asserting { result =>
        result.usedNames should contain key fVfqn
        result.usedNames should contain key gVfqn
      }
  }

  private def sourced[T](value: T): Sourced[T] = Sourced(testFile, PositionRange.zero, value)

  private def valueRef(vfqn: ValueFQN): MonomorphicExpression.MonomorphicValueReference =
    MonomorphicExpression.MonomorphicValueReference(sourced(vfqn), Seq.empty)

  private def runtime(expr: MonomorphicExpression.Expression): Option[Sourced[MonomorphicExpression.Expression]] =
    Some(sourced(expr))

  private def runProcessor(
      key: UsedNames.Key,
      facts: Seq[CompilerFact]
  ): IO[UsedNames] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq(UsedNamesProcessor())))
      _         <- generator.registerFact(sourceContent)
      _         <- facts.traverse_(generator.registerFact)
      result    <- generator.getFact(key)
      errors    <- generator.currentErrors()
      _         <- if (errors.nonEmpty)
                     IO.raiseError(new Exception(s"Errors: ${errors.map(_.message).mkString(", ")}"))
                   else IO.unit
    } yield result.getOrElse(throw new Exception("UsedNames not produced"))
}
