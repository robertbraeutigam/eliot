package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.fact.{MatchDesugaredExpression, MatchDesugaredValue}
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, ModuleName => ModuleName2}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer
import MatchDesugaredExpressionMatchers.*

class MatchDesugaringProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName2.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor()
    ) {
  private val testMN = ModuleName2(Seq.empty, "Test")

  "match desugaring" should "pass through values without match expressions" in {
    runEngineForValue("data T\ndef main: T = 1").asserting {
      case Some(IntLit(v)) => v shouldBe BigInt(1)
      case x               => fail(s"unexpected: $x")
    }
  }

  it should "pass through abstract values without runtime" in {
    runEngineForValue("data T\ndef main: T").asserting(_ shouldBe None)
  }

  it should "desugar match on nullary constructors to handleWith call" in {
    runEngineForValue(
      "data Bool = True | False\ndef main: Bool = True match { case True -> True case False -> False }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(ValRef(hw), ValRef(scrutinee)), FunLit("_", ValRef(trueBody))), FunLit("_", ValRef(falseBody)))) =>
        hw shouldBe vfqn("handleBoolWith")
        scrutinee shouldBe vfqn("True")
        trueBody shouldBe vfqn("True")
        falseBody shouldBe vfqn("False")
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "order handlers by constructor definition order" in {
    runEngineForValue(
      "data Color = Red | Green | Blue\ndef main: Color = Red match { case Blue -> Blue case Red -> Red case Green -> Green }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(FunApp(ValRef(hw), _), FunLit("_", ValRef(first))), FunLit("_", ValRef(second))), FunLit("_", ValRef(third)))) =>
        hw shouldBe vfqn("handleColorWith")
        first shouldBe vfqn("Red")
        second shouldBe vfqn("Green")
        third shouldBe vfqn("Blue")
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar match with constructor field binding" in {
    runEngineForValue(
      "data Maybe = Nothing | Just(value: Maybe)\ndef main: Maybe = Nothing match { case Nothing -> Nothing case Just(v) -> v }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(ValRef(hw), ValRef(scrutinee)), FunLit("_", ValRef(nothingBody))), FunLit("v", ParamRef(paramName)))) =>
        hw shouldBe vfqn("handleMaybeWith")
        scrutinee shouldBe vfqn("Nothing")
        nothingBody shouldBe vfqn("Nothing")
        paramName shouldBe "v"
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar match with multiple constructor fields as curried lambdas" in {
    runEngineForValue(
      "data Pair = MkPair(first: Pair, second: Pair) | Empty\ndef main: Pair = Empty match { case MkPair(a, b) -> a case Empty -> Empty }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(ValRef(hw), ValRef(_)), FunLit("a", FunLit("b", ParamRef(innerParam)))), FunLit("_", ValRef(emptyBody)))) =>
        hw shouldBe vfqn("handlePairWith")
        innerParam shouldBe "a"
        emptyBody shouldBe vfqn("Empty")
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "generate wildcard handlers for unmatched constructors" in {
    runEngineForValue(
      "data Color = Red | Green | Blue\ndef main: Color = Red match { case Red -> Red case _ -> Green }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(FunApp(ValRef(hw), _), FunLit("_", ValRef(redBody))), FunLit("_", ValRef(greenBody))), FunLit("_", ValRef(blueBody)))) =>
        hw shouldBe vfqn("handleColorWith")
        redBody shouldBe vfqn("Red")
        greenBody shouldBe vfqn("Green")
        blueBody shouldBe vfqn("Green")
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "generate wildcard handlers with correct arity for non-nullary constructors" in {
    runEngineForValue(
      "data Maybe = Nothing | Just(value: Maybe)\ndef main: Maybe = Nothing match { case Nothing -> Nothing case _ -> Nothing }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(ValRef(hw), _), FunLit("_", ValRef(nothingBody))), FunLit("_", ValRef(justBody)))) =>
        hw shouldBe vfqn("handleMaybeWith")
        nothingBody shouldBe vfqn("Nothing")
        justBody shouldBe vfqn("Nothing")
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar match expression inside lambda body" in {
    runEngineForValue(
      "data Bool = True | False\ndef main: Bool = b: Bool -> b match { case True -> True case False -> False }"
    ).asserting {
      case Some(FunLit("b", FunApp(FunApp(FunApp(ValRef(hw), ParamRef(scrutinee)), _), _))) =>
        hw shouldBe vfqn("handleBoolWith")
        scrutinee shouldBe "b"
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar embedded constructor patterns as nested matches" in {
    runEngineForValue(
      "data Inner = MkInner\ndata Outer = Wrap(inner: Inner) | Empty\ndef main: Outer = Empty match { case Wrap(MkInner) -> Empty case Empty -> Empty }"
    ).asserting {
      case Some(FunApp(FunApp(FunApp(ValRef(outerHw), _), wrapHandler), FunLit("_", ValRef(emptyBody)))) =>
        outerHw shouldBe vfqn("handleOuterWith")
        emptyBody shouldBe vfqn("Empty")
        wrapHandler match {
          case FunLit("$match_field", FunApp(FunApp(ValRef(innerHw), ParamRef(innerScrutinee)), FunLit("_", ValRef(mkInnerBody)))) =>
            innerHw shouldBe vfqn("handleInnerWith")
            innerScrutinee shouldBe "$match_field"
            mkInnerBody shouldBe vfqn("Empty")
          case x =>
            fail(s"unexpected Wrap handler: $x")
        }
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "report error for non-exhaustive match" in {
    runEngineForErrors(
      "data Color = Red | Green | Blue\ndef main: Color = Red match { case Red -> Red case Blue -> Blue }"
    ).asserting(_ shouldBe Seq("Non-exhaustive match. Missing constructors: Green." at "Red"))
  }

  it should "report error when match has no constructor patterns" in {
    runEngineForErrors(
      "data Bool = True | False\ndef main: Bool = True match { case x -> x }"
    ).asserting(_ shouldBe Seq("Match expression must have at least one constructor pattern." at "x"))
  }

  it should "report error when wildcard-only match has no constructor patterns" in {
    runEngineForErrors(
      "data Bool = True | False\ndef main: Bool = True match { case _ -> True }"
    ).asserting(_ shouldBe Seq("Match expression must have at least one constructor pattern." at "_"))
  }

  it should "desugar type match with single constructor to typeMatch call" in {
    runEngineForValue(
      "data Type\ndata S\ndata Person[NAME: S]\ndef main: S = t: Type -> t match { case Person[name] -> name case _ -> 1 }"
    ).asserting {
      case Some(FunLit("t", FunApp(FunApp(FunApp(ValRef(tm), ParamRef(scrutinee)), FunLit("name", ParamRef(nameParam))), FunLit("_", IntLit(fallback))))) =>
        tm shouldBe vfqn("typeMatchPerson")
        scrutinee shouldBe "t"
        nameParam shouldBe "name"
        fallback shouldBe BigInt(1)
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar type match with multiple constructors as chained typeMatch calls" in {
    runEngineForValue(
      "data Type\ndata S\ndata A[X: S]\ndata B[Y: S]\ndef main: S = t: Type -> t match { case A[x] -> x case B[y] -> y case _ -> 1 }"
    ).asserting {
      case Some(FunLit("t", FunApp(FunApp(FunApp(ValRef(tmA), ParamRef(scrutineeA)), FunLit("x", ParamRef(xParam))), FunLit("_", FunApp(FunApp(FunApp(ValRef(tmB), ParamRef(scrutineeB)), FunLit("y", ParamRef(yParam))), FunLit("_", IntLit(fallback))))))) =>
        tmA shouldBe vfqn("typeMatchA")
        scrutineeA shouldBe "t"
        xParam shouldBe "x"
        tmB shouldBe vfqn("typeMatchB")
        scrutineeB shouldBe "t"
        yParam shouldBe "y"
        fallback shouldBe BigInt(1)
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar type match with nullary type constructor" in {
    runEngineForValue(
      "data Type\ndata S\ndata Foo\ndef main: S = t: Type -> t match { case Foo[] -> 1 case _ -> 2 }"
    ).asserting {
      case Some(FunLit("t", FunApp(FunApp(FunApp(ValRef(tm), ParamRef(scrutinee)), FunLit("_", IntLit(matchBody))), FunLit("_", IntLit(fallback))))) =>
        tm shouldBe vfqn("typeMatchFoo")
        scrutinee shouldBe "t"
        matchBody shouldBe BigInt(1)
        fallback shouldBe BigInt(2)
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "report error when type match has no wildcard case" in {
    runEngineForErrors(
      "data Type\ndata S\ndata Person[NAME: S]\ndef main: S = t: Type -> t match { case Person[name] -> name }"
    ).asserting(_ shouldBe Seq("Type match must have a wildcard case." at "Person"))
  }

  private def vfqn(name: String): ValueFQN =
    ValueFQN(testMN, QualifiedName(name, Qualifier.Default))

  private def runEngineForValue(source: String): IO[Option[MatchDesugaredExpression]] =
    runGenerator(
      source,
      MatchDesugaredValue.Key(ValueFQN(testMN, QualifiedName("main", Qualifier.Default))),
      systemImports
    ).map { case (_, facts) =>
      facts.values
        .collectFirst { case rv: MatchDesugaredValue if rv.vfqn.name == QualifiedName("main", Qualifier.Default) => rv }
        .flatMap(_.runtime.map(_.value))
    }

  private def runEngineForErrors(source: String): IO[Seq[TestError]] =
    runGenerator(
      source,
      MatchDesugaredValue.Key(ValueFQN(testMN, QualifiedName("main", Qualifier.Default))),
      systemImports
    ).map(result => toTestErrors(result._1))
}
