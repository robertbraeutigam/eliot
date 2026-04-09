package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, fullyQualifiedNameType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.EvalIO
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class QuoterTest extends ProcessorTest() {

  "Quoter" should "quote Lit to Direct" in {
    runQuote(Sem.Lit(Value.Direct(42, bigIntType)))
      .asserting(_ shouldBe Some(Value.Direct(42, bigIntType)))
  }

  it should "quote string Lit" in {
    runQuote(Sem.Lit(Value.Direct("hello", stringType)))
      .asserting(_ shouldBe Some(Value.Direct("hello", stringType)))
  }

  it should "quote TypeUniv to Type" in {
    runQuote(Sem.TypeUniv)
      .asserting(_ shouldBe Some(Value.Type))
  }

  it should "quote empty Struct" in {
    val fqn = ValueFQN(testModuleName, QualifiedName("MyType", Qualifier.Type))
    val sem = Sem.Struct(fqn, Map("$typeName" -> Sem.Lit(Value.Direct(fqn, fullyQualifiedNameType))))
    runQuote(sem)
      .asserting {
        case Some(Value.Structure(fields, Value.Type)) =>
          fields.contains("$typeName") shouldBe true
        case other                                      => fail(s"Expected Structure, got $other")
      }
  }

  it should "quote Struct with fields" in {
    val fqn = ValueFQN(testModuleName, QualifiedName("Pair", Qualifier.Type))
    val sem = Sem.Struct(
      fqn,
      Map(
        "$typeName" -> Sem.Lit(Value.Direct(fqn, fullyQualifiedNameType)),
        "A"         -> Sem.Lit(Value.Direct(1, bigIntType)),
        "B"         -> Sem.Lit(Value.Direct("x", stringType))
      )
    )
    runQuote(sem)
      .asserting {
        case Some(Value.Structure(fields, Value.Type)) =>
          fields("A") shouldBe Value.Direct(1, bigIntType)
          fields("B") shouldBe Value.Direct("x", stringType)
        case other                                      => fail(s"Expected Structure, got $other")
      }
  }

  it should "quote nested Struct" in {
    val innerFqn = ValueFQN(testModuleName, QualifiedName("Inner", Qualifier.Type))
    val outerFqn = ValueFQN(testModuleName, QualifiedName("Outer", Qualifier.Type))
    val inner    = Sem.Struct(
      innerFqn,
      Map("$typeName" -> Sem.Lit(Value.Direct(innerFqn, fullyQualifiedNameType)))
    )
    val outer    = Sem.Struct(
      outerFqn,
      Map(
        "$typeName" -> Sem.Lit(Value.Direct(outerFqn, fullyQualifiedNameType)),
        "inner"     -> inner
      )
    )
    runQuote(outer)
      .asserting {
        case Some(Value.Structure(fields, Value.Type)) =>
          fields("inner") shouldBe a[Value.Structure]
        case other                                      => fail(s"Expected nested Structure, got $other")
      }
  }

  it should "return None for free parameter" in {
    runQuote(Sem.Neut(Head.Param("x")))
      .asserting(_ shouldBe None)
  }

  it should "return None for lambda" in {
    val lam = Sem.Lam(
      "x",
      Sem.TypeUniv,
      Closure(Env.empty, OperatorResolvedExpression.ParameterReference(s("x")))
    )
    runQuote(lam)
      .asserting(_ shouldBe None)
  }

  it should "return None for Ref with non-empty spine" in {
    val fqn = ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))
    runQuote(Sem.Neut(Head.Ref(fqn), Seq(Sem.Lit(Value.Direct(1, bigIntType)))))
      .asserting(_ shouldBe None)
  }

  it should "quote bare Ref as type structure" in {
    val fqn = ValueFQN(testModuleName, QualifiedName("MyType", Qualifier.Type))
    runQuote(Sem.Neut(Head.Ref(fqn)))
      .asserting {
        case Some(Value.Structure(fields, Value.Type)) =>
          fields("$typeName") shouldBe Value.Direct(fqn, fullyQualifiedNameType)
        case other                                      => fail(s"Expected Structure for Ref, got $other")
      }
  }

  it should "quote solved meta as its solution" in {
    val sem = Sem.Lit(Value.Direct(42, bigIntType))
    runQuoteWithMeta(sem)
      .asserting(_ shouldBe Some(Value.Direct(42, bigIntType)))
  }

  it should "return None for unsolved meta" in {
    runQuoteWithUnsolvedMeta()
      .asserting(_ shouldBe None)
  }

  // --- Helpers ---

  private def s[T](v: T): Sourced[T] = Sourced(file, PositionRange.zero, v)

  private def runQuote(sem: Sem): IO[Option[Value]] =
    for {
      generator <- createGenerator(Seq.empty)
      result    <- Quoter.quote(sem).runA(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      =>
        throw new Exception(s"Quote failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runQuoteWithMeta(solution: Sem): IO[Option[Value]] = {
    val action: EvalIO[Option[Value]] = for {
      metaSem <- MetaState.freshMeta("test", Sem.TypeUniv)
      metaId   = metaSem match { case Sem.Neut(Head.Meta(id), _) => id; case _ => throw new Exception("not a meta") }
      _       <- MetaState.solveMeta(metaId, solution)
      result  <- Quoter.quote(metaSem)
    } yield result
    for {
      generator <- createGenerator(Seq.empty)
      result    <- action.runA(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      =>
        throw new Exception(s"Quote failed: ${errors.map(_.message).toList.mkString(", ")}")
    }
  }

  private def runQuoteWithUnsolvedMeta(): IO[Option[Value]] = {
    val action: EvalIO[Option[Value]] = for {
      metaSem <- MetaState.freshMeta("test", Sem.TypeUniv)
      result  <- Quoter.quote(metaSem)
    } yield result
    for {
      generator <- createGenerator(Seq.empty)
      result    <- action.runA(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      =>
        throw new Exception(s"Quote failed: ${errors.map(_.message).toList.mkString(", ")}")
    }
  }
}
