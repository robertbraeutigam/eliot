package com.vanillasource.eliot.eliotc.block.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.block.fact.BlockDesugaredValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

class BlockDesugaringProcessorTest extends ProcessorTest(LangProcessors()*) {

  // ── Lowering ──────────────────────────────────────────────────────────────────────────────────────────────────

  "block lowering" should "lower `val x = a; b` to the immediately-applied lambda `(x -> b)(a)`" in {
    lowered("def a: String\ndef b: String\ndef f: String = {\n  val x = a\n  b\n}", "f").asserting {
      case Some(RApp(RLit(param, RVal(body)), RVal(arg))) => (param, body, arg) shouldBe ("x", "b", "a")
      case other                                          => fail(s"unexpected: $other")
    }
  }

  it should "lower a bare statement to an immediately-applied lambda with a discarded `_` binder" in {
    lowered("def a: String\ndef b: String\ndef f: String = {\n  a\n  b\n}", "f").asserting {
      case Some(RApp(RLit("_", RVal("b")), RVal("a"))) => succeed
      case other                                       => fail(s"unexpected: $other")
    }
  }

  it should "preserve a typed binder's annotation on the lowered lambda parameter" in {
    lowered("def a: String\ndef b: String\ndef f: String = {\n  val x: String = a\n  b\n}", "f").asserting {
      case Some(RApp(RLitTyped("x", true), RVal("a"))) => succeed
      case other                                       => fail(s"unexpected: $other")
    }
  }

  it should "lower three statements into a left-nested tower of immediately-applied lambdas" in {
    lowered("def a: String\ndef b: String\ndef c: String\ndef f: String = {\n  a\n  b\n  c\n}", "f").asserting {
      case Some(RApp(RLit("_", RApp(RLit("_", RVal("c")), RVal("b"))), RVal("a"))) => succeed
      case other                                                                   => fail(s"unexpected: $other")
    }
  }

  it should "lower a nested block in a val's right-hand side, lowering both blocks" in {
    lowered("def a: String\ndef b: String\ndef f: String = {\n  val x = {\n    a\n    b\n  }\n  x\n}", "f").asserting {
      case Some(RApp(RLit("x", RParam("x")), RApp(RLit("_", RVal("b")), RVal("a")))) => succeed
      case other                                                                     => fail(s"unexpected: $other")
    }
  }

  // NOTE: The former "effect threading end to end" cases (blocks lifting to flatMap/map towers) migrated to
  // `MonomorphicTypeCheckTest`'s effect-lift section: the auto-lift is type-directed elaboration in the NbE checker
  // now (docs/effect-lift-in-checker.md), so the sequenced shape materialises in the *monomorphic* body, not here.

  // ── Merge by fixity ───────────────────────────────────────────────────────────────────────────────────────────

  "block merge" should "join leading-dot continuations into one expression (lower line starts with an infix `.`)" in {
    lowered(merge("aa\n  .bb\n  .cc"), "f").asserting {
      case Some(RFlat(parts)) => parts.collect { case RVal(n) => n } shouldBe Seq("aa", ".", "bb", ".", "cc")
      case other              => fail(s"unexpected: $other")
    }
  }

  it should "join a line ending in an infix operator with the next line" in {
    lowered(merge("aa plus\n  bb"), "f").asserting {
      case Some(RFlat(parts)) => parts.collect { case RVal(n) => n } shouldBe Seq("aa", "plus", "bb")
      case other              => fail(s"unexpected: $other")
    }
  }

  it should "join when the lower line starts with an alphanumeric infix operator" in {
    lowered(merge("aa\n  oor bb"), "f").asserting {
      case Some(RFlat(parts)) => parts.collect { case RVal(n) => n } shouldBe Seq("aa", "oor", "bb")
      case other              => fail(s"unexpected: $other")
    }
  }

  it should "NOT merge when the lower line starts with a prefix operator (two statements)" in {
    lowered(merge("aa\n  neg bb"), "f").asserting {
      case Some(RApp(RLit("_", RFlat(_)), RVal("aa"))) => succeed
      case other                                       => fail(s"unexpected: $other")
    }
  }

  it should "keep a plain application sequence as separate statements" in {
    lowered(merge("aa\n  bb\n  cc"), "f").asserting {
      case Some(RApp(RLit("_", RApp(RLit("_", RVal("cc")), RVal("bb"))), RVal("aa"))) => succeed
      case other                                                                     => fail(s"unexpected: $other")
    }
  }

  it should "never merge across a blank line, even where fixity would otherwise join" in {
    lowered(merge("aa plus\n\n  bb"), "f").asserting {
      case Some(RApp(RLit("_", RVal("bb")), RFlat(_))) => succeed
      case other                                       => fail(s"unexpected: $other")
    }
  }

  it should "never merge a line into a following `val` binding, even where fixity would otherwise join" in {
    lowered(merge("aa oor\n  val v = bb\n  v"), "f").asserting {
      case Some(RApp(RLit("_", RApp(RLit("v", RParam("v")), RVal("bb"))), RFlat(_))) => succeed
      case other                                                                    => fail(s"unexpected: $other")
    }
  }

  // ── Errors ────────────────────────────────────────────────────────────────────────────────────────────────────

  "block errors" should "reject a self-reference in a val's own right-hand side" in {
    blockErrors("def idfun(a: String): String\ndef f: String = {\n  val x = idfun x\n  x\n}", "f")
      .asserting(_ should contain("'x' is referenced in its own definition."))
  }

  it should "reject an empty block" in {
    blockErrors("def f: String = {\n}", "f")
      .asserting(_ should contain("A block must contain at least one expression."))
  }

  it should "reject a block ending in a binding" in {
    blockErrors("def a: String\ndef f: String = {\n  val x = a\n}", "f")
      .asserting(_ should contain("A block must end in an expression, not a binding."))
  }

  // ── Termination ───────────────────────────────────────────────────────────────────────────────────────────────

  "block termination" should "pass the recursion check for a non-recursive block" in {
    recursionErrors("def aa: String\ndef f: String = {\n  val x = aa\n  x\n}", "f").asserting(_ shouldBe Seq.empty)
  }

  it should "still flag a value that is self-referential through a block" in {
    recursionErrors("def f: String = {\n  val x = f\n  x\n}", "f")
      .asserting(_ should contain("Value 'f' is defined recursively."))
  }

  // ── Helpers ───────────────────────────────────────────────────────────────────────────────────────────────────

  /** Common operator/value declarations plus a `def f` whose body is the given block source. */
  private def merge(blockBody: String): String =
    """def aa: String
      |def bb: String
      |def cc: String
      |infix def .(p: String, q: String): String
      |infix def plus(p: String, q: String): String
      |infix def oor(p: String, q: String): String
      |prefix def neg(p: String): String
      |""".stripMargin + s"def f: String = {\n  $blockBody\n}"

  private val imports = systemImports

  private def vfqn(name: String): ValueFQN = ValueFQN(testModuleName, QualifiedName(name, Qualifier.Default))

  private def lowered(source: String, name: String): IO[Option[Expression]] =
    runGenerator(source, BlockDesugaredValue.Key(vfqn(name)), imports).map { case (_, facts) =>
      facts.values
        .collectFirst { case bdv: BlockDesugaredValue if bdv.vfqn == vfqn(name) => bdv }
        .flatMap(_.runtime.map(_.value))
    }

  private def blockErrors(source: String, name: String): IO[Seq[String]] =
    runGenerator(source, BlockDesugaredValue.Key(vfqn(name)), imports).map(_._1.map(_.message))

  private def recursionErrors(source: String, name: String): IO[Seq[String]] =
    runGenerator(source, RecursionCheckedValue.Key(vfqn(name)), imports).map(_._1.map(_.message))

  // ── Matchers for the resolved (block-desugared) expression model ──────────────────────────────────────────────

  private object RApp {
    def unapply(e: Expression): Option[(Expression, Expression)] = e match {
      case Expression.FunctionApplication(Sourced(_, _, target), Sourced(_, _, arg)) => Some((target, arg))
      case _                                                                         => None
    }
  }

  private object RLit {
    def unapply(e: Expression): Option[(String, Expression)] = e match {
      case Expression.FunctionLiteral(Sourced(_, _, param), _, Sourced(_, _, body)) => Some((param, body.signature))
      case _                                                                        => None
    }
  }

  /** Like [[RLit]] but exposes whether the lambda parameter carries a (binder) type annotation, instead of the body. */
  private object RLitTyped {
    def unapply(e: Expression): Option[(String, Boolean)] = e match {
      case Expression.FunctionLiteral(Sourced(_, _, param), paramType, _) => Some((param, paramType.isDefined))
      case _                                                              => None
    }
  }

  private object RFlat {
    def unapply(e: Expression): Option[Seq[Expression]] = e match {
      case Expression.FlatExpression(parts) => Some(parts.map(_.value.signature))
      case _                                => None
    }
  }

  private object RVal {
    def unapply(e: Expression): Option[String] = e match {
      case Expression.ValueReference(Sourced(_, _, fqn), _) => Some(fqn.name.name)
      case _                                                => None
    }
  }

  private object RParam {
    def unapply(e: Expression): Option[String] = e match {
      case Expression.ParameterReference(Sourced(_, _, name)) => Some(name)
      case _                                                  => None
    }
  }
}
