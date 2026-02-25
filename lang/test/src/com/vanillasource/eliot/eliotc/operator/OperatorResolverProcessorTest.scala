package com.vanillasource.eliot.eliotc.operator

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, ModuleName as ModuleName2}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.operator.{OperatorResolvedValue, OperatorResolverProcessor}
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedExpressionMatchers.*
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class OperatorResolverProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName2.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      OperatorResolverProcessor()
    ) {
  private val testModuleName2    = ModuleName2(Seq.empty, "Test")
  private val functionModuleName = ModuleName2.systemFunctionModuleName

  "operator resolver" should "resolve space application f a b as f(a)(b)" in {
    runEngineForValue("data T\ndef f: T\ndef a: T\ndef b: T\ndef main: T = f a b").asserting {
      case Some(FunApp(FunApp(ValRef(fVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        fVfqn shouldBe vfqn("f")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve simple infix a + b as +(a)(b)" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ndef a: T\ndef b: T\ndef main: T = a + b"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(opVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        opVfqn shouldBe vfqn("+")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve identifier infix a or b as or(a)(b)" in {
    runEngineForValue(
      "data T\ninfix left def or(x: T, y: T): T\ndef a: T\ndef b: T\ndef main: T = a or b"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(opVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        opVfqn shouldBe vfqn("or")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve precedence a + b * c with * above + as +(a, *(b, c))" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ninfix left above(+) def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plusVfqn), ValRef(aVfqn)), FunApp(FunApp(ValRef(timesVfqn), ValRef(bVfqn)), ValRef(cVfqn)))) =>
        plusVfqn shouldBe vfqn("+")
        timesVfqn shouldBe vfqn("*")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
        cVfqn shouldBe vfqn("c")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve left-associative a + b + c as (a + b) + c" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b + c"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plus2), FunApp(FunApp(ValRef(plus1), ValRef(aVfqn)), ValRef(bVfqn))), ValRef(cVfqn))) =>
        plus1 shouldBe vfqn("+")
        plus2 shouldBe vfqn("+")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
        cVfqn shouldBe vfqn("c")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve right-associative a cons b cons c as cons(a, cons(b, c))" in {
    runEngineForValue(
      "data T\ninfix right def cons(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a cons b cons c"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(cons1), ValRef(aVfqn)), FunApp(FunApp(ValRef(cons2), ValRef(bVfqn)), ValRef(cVfqn)))) =>
        cons1 shouldBe vfqn("cons")
        cons2 shouldBe vfqn("cons")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
        cVfqn shouldBe vfqn("c")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve prefix before infix: !x + y as +(!(x), y)" in {
    runEngineForValue(
      "data T\nprefix def !(x: T): T\ninfix left def +(x: T, y: T): T\ndef x: T\ndef y: T\ndef main: T = ! x + y"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plusVfqn), FunApp(ValRef(notVfqn), ValRef(xVfqn))), ValRef(yVfqn))) =>
        plusVfqn shouldBe vfqn("+")
        notVfqn shouldBe vfqn("!")
        xVfqn shouldBe vfqn("x")
        yVfqn shouldBe vfqn("y")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve postfix before infix: x ++ + y as +((x)++, y)" in {
    runEngineForValue(
      "data T\npostfix def ++(x: T): T\ninfix left def +(x: T, y: T): T\ndef x: T\ndef y: T\ndef main: T = x ++ + y"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plusVfqn), FunApp(ValRef(incVfqn), ValRef(xVfqn))), ValRef(yVfqn))) =>
        plusVfqn shouldBe vfqn("+")
        incVfqn shouldBe vfqn("++")
        xVfqn shouldBe vfqn("x")
        yVfqn shouldBe vfqn("y")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve mixed space and infix: f a + g b as +(f(a), g(b))" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ndef f: T\ndef g: T\ndef a: T\ndef b: T\ndef main: T = f a + g b"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plusVfqn), FunApp(ValRef(fVfqn), ValRef(aVfqn))), FunApp(ValRef(gVfqn), ValRef(bVfqn)))) =>
        plusVfqn shouldBe vfqn("+")
        fVfqn shouldBe vfqn("f")
        gVfqn shouldBe vfqn("g")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "error on two infix ops without relative precedence" in {
    runEngineForErrors(
      "data T\ninfix left def +(x: T, y: T): T\ninfix left def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
    ).asserting(_.head should include("no defined relative precedence"))
  }

  it should "error on non-associative operator chained" in {
    runEngineForErrors(
      "data T\ninfix none def ==(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a == b == c"
    ).asserting(_.head should include("cannot be chained"))
  }

  it should "resolve parenthesized call regardless of fixity" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ndef a: T\ndef b: T\ndef main: T = +(a, b)"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(opVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        opVfqn shouldBe vfqn("+")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "pass through single atom expressions" in {
    runEngineForValue("data T\ndef a: T\ndef main: T = a").asserting {
      case Some(ValRef(aVfqn)) => aVfqn shouldBe vfqn("a")
      case x                   => fail(s"unexpected: $x")
    }
  }

  it should "pass through literal expressions" in {
    runEngineForValue("data T\ndef main: T = 42").asserting {
      case Some(IntLit(v)) => v shouldBe BigInt(42)
      case x               => fail(s"unexpected: $x")
    }
  }

  private def vfqn(name: String): ValueFQN =
    ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))

  private def runEngineForValue(source: String): IO[Option[OperatorResolvedExpression]] =
    runGenerator(
      source,
      OperatorResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("main", Qualifier.Default))),
      systemImports
    ).map { case (_, facts) =>
      facts.values
        .collectFirst { case rv: OperatorResolvedValue if rv.vfqn.name == QualifiedName("main", Qualifier.Default) => rv }
        .flatMap(_.runtime.map(_.value))
    }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(
      source,
      OperatorResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("main", Qualifier.Default))),
      systemImports
    ).map(_._1.map(_.message))
}
