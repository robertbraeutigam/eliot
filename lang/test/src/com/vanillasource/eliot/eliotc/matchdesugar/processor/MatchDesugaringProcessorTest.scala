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

  override val systemImports = Seq(
    SystemImport("Function", "data Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport(
      "PatternMatch",
      "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}"
    )
  )

  "match desugaring" should "pass through values without match expressions" in {
    runEngineForValue("data T\ndef main: T = 1").asserting {
      case Some(IntLit(v)) => v shouldBe BigInt(1)
      case x               => fail(s"unexpected: $x")
    }
  }

  it should "pass through abstract values without runtime" in {
    runEngineForValue("data T\ndef main: T").asserting(_ shouldBe None)
  }

  it should "desugar match on nullary constructors to handleCases call" in {
    runEngineForValue(
      "data Bool = True | False\ndef main: Bool = True match { case True -> True case False -> False }"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(hc), ValRef(scrutinee)), FunLit("$selector", FunApp(FunApp(ParamRef(sel), FunLit("_", ValRef(trueBody))), FunLit("_", ValRef(falseBody)))))) =>
        isHandleCases(hc) shouldBe true
        scrutinee shouldBe vfqn("True")
        sel shouldBe "$selector"
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
      case Some(FunApp(FunApp(ValRef(hc), _), FunLit("$selector", FunApp(FunApp(FunApp(ParamRef(sel), FunLit("_", ValRef(first))), FunLit("_", ValRef(second))), FunLit("_", ValRef(third)))))) =>
        isHandleCases(hc) shouldBe true
        sel shouldBe "$selector"
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
      case Some(FunApp(FunApp(ValRef(hc), ValRef(scrutinee)), FunLit("$selector", FunApp(FunApp(ParamRef(sel), FunLit("_", ValRef(nothingBody))), FunLit("v", ParamRef(paramName)))))) =>
        isHandleCases(hc) shouldBe true
        scrutinee shouldBe vfqn("Nothing")
        sel shouldBe "$selector"
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
      case Some(FunApp(FunApp(ValRef(hc), ValRef(_)), FunLit("$selector", FunApp(FunApp(ParamRef(sel), FunLit("a", FunLit("b", ParamRef(innerParam)))), FunLit("_", ValRef(emptyBody)))))) =>
        isHandleCases(hc) shouldBe true
        sel shouldBe "$selector"
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
      case Some(FunApp(FunApp(ValRef(hc), _), FunLit("$selector", FunApp(FunApp(FunApp(ParamRef(sel), FunLit("_", ValRef(redBody))), FunLit("_", ValRef(greenBody))), FunLit("_", ValRef(blueBody)))))) =>
        isHandleCases(hc) shouldBe true
        sel shouldBe "$selector"
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
      case Some(FunApp(FunApp(ValRef(hc), _), FunLit("$selector", FunApp(FunApp(ParamRef(sel), FunLit("_", ValRef(nothingBody))), FunLit("_", ValRef(justBody)))))) =>
        isHandleCases(hc) shouldBe true
        sel shouldBe "$selector"
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
      case Some(FunLit("b", FunApp(FunApp(ValRef(hc), ParamRef(scrutinee)), FunLit("$selector", _)))) =>
        isHandleCases(hc) shouldBe true
        scrutinee shouldBe "b"
      case x =>
        fail(s"unexpected: $x")
    }
  }

  it should "desugar embedded constructor patterns as nested matches" in {
    runEngineForValue(
      "data Inner = MkInner\ndata Outer = Wrap(inner: Inner) | Empty\ndef main: Outer = Empty match { case Wrap(MkInner) -> Empty case Empty -> Empty }"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(outerHc), _), FunLit("$selector", FunApp(FunApp(ParamRef(outerSel), wrapHandler), FunLit("_", ValRef(emptyBody)))))) =>
        isHandleCases(outerHc) shouldBe true
        outerSel shouldBe "$selector"
        emptyBody shouldBe vfqn("Empty")
        wrapHandler match {
          case FunLit("$match_field", FunApp(FunApp(ValRef(innerHc), ParamRef(innerScrutinee)), FunLit("$selector", FunApp(ParamRef(innerSel), FunLit("_", ValRef(mkInnerBody)))))) =>
            isHandleCases(innerHc) shouldBe true
            innerScrutinee shouldBe "$match_field"
            innerSel shouldBe "$selector"
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

  private def isHandleCases(vfqn: ValueFQN): Boolean =
    vfqn.moduleName == testMN &&
      vfqn.name.name == "handleCases" &&
      (vfqn.name.qualifier match {
        case Qualifier.AbilityImplementation(abilityName, _) => abilityName.value == "PatternMatch"
        case _                                               => false
      })

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
