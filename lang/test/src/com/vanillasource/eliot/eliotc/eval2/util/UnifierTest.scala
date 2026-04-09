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

class UnifierTest extends ProcessorTest() {

  "Unifier" should "unify identical literals" in {
    runUnify(intSem(42), intSem(42))
      .asserting(_ shouldBe Right(()))
  }

  it should "fail on different literals" in {
    runUnify(intSem(1), intSem(2))
      .asserting(_ shouldBe a[Left[?, ?]])
  }

  it should "unify TypeUniv with TypeUniv" in {
    runUnify(Sem.TypeUniv, Sem.TypeUniv)
      .asserting(_ shouldBe Right(()))
  }

  it should "fail on TypeUniv vs Lit" in {
    runUnify(Sem.TypeUniv, intSem(42))
      .asserting(_ shouldBe a[Left[?, ?]])
  }

  it should "unify identical Structs" in {
    val fqn = testFQN("T")
    val s1  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(1)))
    val s2  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(1)))
    runUnify(s1, s2)
      .asserting(_ shouldBe Right(()))
  }

  it should "fail on Structs with different type constructors" in {
    val s1 = Sem.Struct(testFQN("T1"), Map("$typeName" -> typeName(testFQN("T1"))))
    val s2 = Sem.Struct(testFQN("T2"), Map("$typeName" -> typeName(testFQN("T2"))))
    runUnify(s1, s2)
      .asserting(_ shouldBe a[Left[?, ?]])
  }

  it should "unify Structs with nested Sem fields" in {
    val fqn = testFQN("Pair")
    val s1  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(1), "B" -> strSem("x")))
    val s2  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(1), "B" -> strSem("x")))
    runUnify(s1, s2)
      .asserting(_ shouldBe Right(()))
  }

  it should "fail on Structs with different field values" in {
    val fqn = testFQN("T")
    val s1  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(1)))
    val s2  = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(2)))
    runUnify(s1, s2)
      .asserting(_ shouldBe a[Left[?, ?]])
  }

  it should "unify identical neutral terms" in {
    val n1 = Sem.Neut(Head.Param("x"))
    val n2 = Sem.Neut(Head.Param("x"))
    runUnify(n1, n2)
      .asserting(_ shouldBe Right(()))
  }

  it should "unify neutral terms with matching spines" in {
    val n1 = Sem.Neut(Head.Param("f"), Seq(intSem(1)))
    val n2 = Sem.Neut(Head.Param("f"), Seq(intSem(1)))
    runUnify(n1, n2)
      .asserting(_ shouldBe Right(()))
  }

  it should "fail on neutral terms with different heads" in {
    val n1 = Sem.Neut(Head.Param("x"))
    val n2 = Sem.Neut(Head.Param("y"))
    runUnify(n1, n2)
      .asserting(_ shouldBe a[Left[?, ?]])
  }

  it should "solve meta against concrete value" in {
    runUnifyWithMeta { metaSem =>
      for {
        _ <- Unifier.unify(metaSem, intSem(42), src)
      } yield metaSem
    }.asserting { case (result, state) =>
      result shouldBe a[Right[?, ?]]
      val meta = result.toOption.get match {
        case Sem.Neut(Head.Meta(id), _) => id
        case _                           => fail("not a meta")
      }
      state.metas(meta).solution shouldBe Some(intSem(42))
    }
  }

  it should "solve meta against Struct" in {
    val fqn    = testFQN("T")
    val struct = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn)))
    runUnifyWithMeta { metaSem =>
      for {
        _ <- Unifier.unify(metaSem, struct, src)
      } yield metaSem
    }.asserting { case (result, state) =>
      result shouldBe a[Right[?, ?]]
      val meta = result.toOption.get match {
        case Sem.Neut(Head.Meta(id), _) => id
        case _                           => fail("not a meta")
      }
      state.metas(meta).solution shouldBe Some(struct)
    }
  }

  it should "solve meta symmetrically (meta on right)" in {
    runUnifyWithMeta { metaSem =>
      for {
        _ <- Unifier.unify(intSem(42), metaSem, src)
      } yield metaSem
    }.asserting { case (result, state) =>
      result shouldBe a[Right[?, ?]]
      val meta = result.toOption.get match {
        case Sem.Neut(Head.Meta(id), _) => id
        case _                           => fail("not a meta")
      }
      state.metas(meta).solution shouldBe Some(intSem(42))
    }
  }

  it should "solve two metas to the same value through transitive unification" in {
    val action: EvalIO[(Sem, Sem)] = for {
      m1 <- MetaState.freshMeta("a", Sem.TypeUniv)
      m2 <- MetaState.freshMeta("b", Sem.TypeUniv)
      _  <- Unifier.unify(m1, intSem(42), src)
      _  <- Unifier.unify(m1, m2, src)
    } yield (m1, m2)
    runAction(action)
      .asserting { case (result, state) =>
        result shouldBe a[Right[?, ?]]
        val (m1Sem, m2Sem) = result.toOption.get
        val m2Id           = m2Sem match { case Sem.Neut(Head.Meta(id), _) => id; case _ => fail("not meta") }
        state.metas(m2Id).solution shouldBe Some(intSem(42))
      }
  }

  it should "detect occurs check (infinite type)" in {
    val action: EvalIO[Unit] = for {
      metaSem <- MetaState.freshMeta("a", Sem.TypeUniv)
      fqn      = testFQN("T")
      struct   = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "inner" -> metaSem))
      _       <- Unifier.unify(metaSem, struct, src)
    } yield ()
    runAction(action)
      .asserting { case (result, _) =>
        result shouldBe a[Left[?, ?]]
      }
  }

  it should "unify through solved meta" in {
    val action: EvalIO[Unit] = for {
      metaSem <- MetaState.freshMeta("a", Sem.TypeUniv)
      _       <- Unifier.unify(metaSem, intSem(42), src)
      _       <- Unifier.unify(metaSem, intSem(42), src)
    } yield ()
    runAction(action)
      .asserting { case (result, _) => result shouldBe Right(()) }
  }

  it should "fail when solved meta conflicts with new unification" in {
    val action: EvalIO[Unit] = for {
      metaSem <- MetaState.freshMeta("a", Sem.TypeUniv)
      _       <- Unifier.unify(metaSem, intSem(42), src)
      _       <- Unifier.unify(metaSem, intSem(99), src)
    } yield ()
    runAction(action)
      .asserting { case (result, _) => result shouldBe a[Left[?, ?]] }
  }

  it should "unify metas nested in Structs" in {
    val fqn = testFQN("T")
    val action: EvalIO[Sem] = for {
      metaSem <- MetaState.freshMeta("a", Sem.TypeUniv)
      s1       = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> metaSem))
      s2       = Sem.Struct(fqn, Map("$typeName" -> typeName(fqn), "A" -> intSem(42)))
      _       <- Unifier.unify(s1, s2, src)
    } yield metaSem
    runAction(action)
      .asserting { case (result, state) =>
        result shouldBe a[Right[?, ?]]
        val metaId = result.toOption.get match {
          case Sem.Neut(Head.Meta(id), _) => id
          case _                           => fail("not meta")
        }
        state.metas(metaId).solution shouldBe Some(intSem(42))
      }
  }

  // --- Helpers ---

  private val src: Sourced[Unit] = Sourced(file, PositionRange.zero, ())

  private def testFQN(name: String): ValueFQN =
    ValueFQN(testModuleName, QualifiedName(name, Qualifier.Type))

  private def intSem(v: BigInt): Sem = Sem.Lit(Value.Direct(v, bigIntType))

  private def strSem(v: String): Sem = Sem.Lit(Value.Direct(v, stringType))

  private def typeName(fqn: ValueFQN): Sem = Sem.Lit(Value.Direct(fqn, fullyQualifiedNameType))

  private def runUnify(s1: Sem, s2: Sem): IO[Either[String, Unit]] = {
    val action: EvalIO[Unit] = Unifier.unify(s1, s2, src)
    runAction(action).map { case (result, _) => result }
  }

  private def runUnifyWithMeta(
      f: Sem => EvalIO[Sem]
  ): IO[(Either[String, Sem], MetaState)] = {
    val action: EvalIO[Sem] = for {
      metaSem <- MetaState.freshMeta("test", Sem.TypeUniv)
      result  <- f(metaSem)
    } yield result
    runAction(action)
  }

  private def runAction[A](action: EvalIO[A]): IO[(Either[String, A], MetaState)] =
    for {
      generator <- createGenerator(Seq.empty)
      result    <- action.run(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, (state, value))) => (Right(value), state)
      case Left(errors)               =>
        (Left(errors.map(_.message).toList.mkString("; ")), MetaState())
    }
}
