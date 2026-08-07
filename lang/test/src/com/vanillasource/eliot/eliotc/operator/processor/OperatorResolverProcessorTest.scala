package com.vanillasource.eliot.eliotc.operator.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, ModuleName as ModuleName2}
import OperatorResolvedExpressionMatchers.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.source.content.Sourced

class OperatorResolverProcessorTest
    extends ProcessorTest(LangProcessors(systemModules = Seq(ModuleName2.systemFunctionModuleName))*) {
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

  // The adjacency rule: a *parenthesized* right operand stays a separate atom (the `(` is not adjacent to `or`), so the
  // infix operator binds it rather than the `(` being read as `or`'s call parens. This is what lets an infix utility
  // take a parenthesized lambda operand, e.g. `result catch (err -> fallback)`.
  it should "resolve infix with a parenthesized operand a or (b) as or(a)(b)" in {
    runEngineForValue(
      "data T\ninfix left def or(x: T, y: T): T\ndef a: T\ndef b: T\ndef main: T = a or (b)"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(opVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        opVfqn shouldBe vfqn("or")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  // The flip side: a *non-infix* name followed by a space-separated parenthesized atom still reduces to ordinary
  // application `f(a)` via operand currying, so making call parens adjacency-sensitive does not change plain calls.
  it should "resolve space application f (a) as f(a)" in {
    runEngineForValue("data T\ndef f(x: T): T\ndef a: T\ndef main: T = f (a)").asserting {
      case Some(FunApp(ValRef(fVfqn), ValRef(aVfqn))) =>
        fVfqn shouldBe vfqn("f")
        aVfqn shouldBe vfqn("a")
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
    ).asserting(_ shouldBe Seq("Operators '*' and '+' have no defined relative precedence." at "*"))
  }

  it should "error on non-associative operator chained" in {
    runEngineForErrors(
      "data T\ninfix none def ==(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a == b == c"
    ).asserting(_ shouldBe Seq("Non-associative operator '==' cannot be chained." at "=="))
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

  it should "pass through the integerLiteral application desugared from a value-position literal" in {
    runEngineForValue("data T\ndef integerLiteral[V]: T\ndef main: T = 42").asserting {
      case Some(OperatorResolvedExpression.ValueReference(Sourced(_, _, refVfqn), Seq(Sourced(_, _, IntLit(v))))) =>
        (refVfqn, v) shouldBe (vfqn("integerLiteral"), BigInt(42))
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve operator declared below apply" in {
    runEngineForValue(
      "data T\ninfix left below apply def +(x: T, y: T): T\ndef a: T\ndef b: T\ndef main: T = a + b"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(opVfqn), ValRef(aVfqn)), ValRef(bVfqn))) =>
        opVfqn shouldBe vfqn("+")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve a below-apply operator tighter than an unrelated floating operator" in {
    runEngineForValue(
      "data T\ninfix left def +(x: T, y: T): T\ninfix left below apply def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plus), ValRef(a)), FunApp(FunApp(ValRef(times), ValRef(b)), ValRef(c)))) =>
        (plus, times, a, b, c) shouldBe (vfqn("+"), vfqn("*"), vfqn("a"), vfqn("b"), vfqn("c"))
      case x => fail(s"unexpected: $x")
    }
  }

  // Robert's nuance: `+` is transitively tied into apply's island (`+ above p`, `p below apply`), so even though
  // `p` never appears in the expression, `+` and `*` are *both* anchored and keep no relative ordering — the pair
  // must still error rather than the below-apply `*` winning by default.
  it should "error when a below-apply operator meets one only transitively connected to apply" in {
    runEngineForErrors(
      "data T\ninfix left below apply def *(x: T, y: T): T\ninfix left below apply def p(x: T, y: T): T\ninfix left above(p) def +(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
    ).asserting(_ shouldBe Seq("Operators '*' and '+' have no defined relative precedence." at "*"))
  }

  it should "error on two operators both below apply without relative precedence" in {
    runEngineForErrors(
      "data T\ninfix left below apply def +(x: T, y: T): T\ninfix left below apply def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
    ).asserting(_ shouldBe Seq("Operators '*' and '+' have no defined relative precedence." at "*"))
  }

  it should "resolve below apply combined with above(+)" in {
    runEngineForValue(
      "data T\ninfix left below apply def +(x: T, y: T): T\ninfix left above(+) def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
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

  it should "error on above apply" in {
    runEngineForErrors(
      "data T\ninfix left above apply def +(x: T, y: T): T\ndef a: T\ndef main: T = a + a"
    ).asserting(_ shouldBe Seq("Infix operator cannot have higher precedence than application." at "apply"))
  }

  it should "error on at apply" in {
    runEngineForErrors(
      "data T\ninfix left at apply def +(x: T, y: T): T\ndef a: T\ndef main: T = a + a"
    ).asserting(_ shouldBe Seq("Infix operator cannot have the same precedence as application." at "apply"))
  }

  it should "resolve application higher than below-apply infix" in {
    runEngineForValue(
      "data T\ninfix left below apply def +(x: T, y: T): T\ndef f: T\ndef a: T\ndef b: T\ndef main: T = f a + b"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(plusVfqn), FunApp(ValRef(fVfqn), ValRef(aVfqn))), ValRef(bVfqn))) =>
        plusVfqn shouldBe vfqn("+")
        fVfqn shouldBe vfqn("f")
        aVfqn shouldBe vfqn("a")
        bVfqn shouldBe vfqn("b")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve below apply with precedence chain" in {
    runEngineForValue(
      "data T\ninfix left below apply def +(x: T, y: T): T\ninfix left below apply above(+) def *(x: T, y: T): T\ndef a: T\ndef b: T\ndef c: T\ndef main: T = a + b * c"
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

  it should "resolve space application in type arguments" in {
    runEngineForValue("data T\ndef f(x: T): T\ndef a: T\ndef g[X: T]: T\ndef main: T = g[f a]").asserting {
      case Some(
            OperatorResolvedExpression
              .ValueReference(Sourced(_, _, gVfqn), Seq(Sourced(_, _, FunApp(ValRef(fVfqn), ValRef(aVfqn)))))
          ) =>
        gVfqn shouldBe vfqn("g")
        fVfqn shouldBe vfqn("f")
        aVfqn shouldBe vfqn("a")
      case x => fail(s"unexpected: $x")
    }
  }

  it should "resolve space application in value type stack" in {
    runEngineForValue("data T\ndef f(x: T): T\ndef a: T\ndata Box[X: T]\ndef main(x: Box[f a]): T = a").asserting {
      case Some(FunLit("x", ValRef(aVfqn))) => aVfqn shouldBe vfqn("a")
      case x                                => fail(s"unexpected: $x")
    }
  }

  it should "resolve space application in lambda parameter type" in {
    runEngineForValue("data T\ndef f(x: T): T\ndef a: T\ndata Box[X: T]\ndef main: T = (x: Box[f a]) -> a")
      .asserting {
        case Some(FunLit("x", ValRef(aVfqn))) => aVfqn shouldBe vfqn("a")
        case x                                => fail(s"unexpected: $x")
      }
  }

  // --- effect-set sugar `{E} A` (effects M1) ---

  "effect-set sugar" should "resolve {E} A to the same signature as the hand-written carrier form" in {
    val source =
      "data Str\ndata Unt\nability Suspend[F[_]] { def delay(value: Str): F[Str] }\n" +
        "def sugar(x: {Suspend} Str): {Suspend} Unt\ndef hand[F[_] ~ Suspend](x: F[Str]): F[Unt]"
    (runEngineForResolvedValue(source, "sugar"), runEngineForResolvedValue(source, "hand")).mapN { (sugar, hand) =>
      (signatureShow(sugar), constraintShow(sugar)) shouldBe (signatureShow(hand), constraintShow(hand))
    }
  }

  it should "resolve {Suspend, Abort} and {Abort, Suspend} to the same signature and effect set" in {
    val source =
      "data Str\nability Suspend[F[_]] { def s(value: Str): F[Str] }\nability Abort[F[_]] { def a(value: Str): F[Str] }\n" +
        "def ab(x: {Suspend, Abort} Str): Str\ndef ba(x: {Abort, Suspend} Str): Str"
    (runEngineForResolvedValue(source, "ab"), runEngineForResolvedValue(source, "ba")).mapN { (ab, ba) =>
      (signatureShow(ab), constraintShow(ab).view.mapValues(_.toSet).toMap) shouldBe
        (signatureShow(ba), constraintShow(ba).view.mapValues(_.toSet).toMap)
    }
  }

  // --- effects-as-channel Phase 1: the declared row is recorded as inert signature metadata (docs/effects-as-channel.md
  // §4). The carrier desugar still runs (asserted above), and *in parallel* the open rows are captured, position-
  // attributed, on `OperatorResolvedValue.effectRow`. ---

  "the effect channel metadata" should "record open rows by position, resolved to ability FQNs" in {
    val source =
      "data Str\ndata Unt\nability Suspend[F[_]] { def delay(value: Str): F[Str] }\n" +
        "def sugar(x: {Suspend} Str): {Suspend} Unt"
    runEngineForResolvedValue(source, "sugar").asserting { sugar =>
      (effectRowReturn(sugar), effectRowParameters(sugar)) shouldBe (Seq("Suspend"), Seq((0, Seq("Suspend"))))
    }
  }

  it should "stay empty for the hand-written carrier form (only the `{…}` sugar feeds the channel)" in {
    val source =
      "data Str\ndata Unt\nability Suspend[F[_]] { def delay(value: Str): F[Str] }\n" +
        "def hand[F[_] ~ Suspend](x: F[Str]): F[Unt]"
    runEngineForResolvedValue(source, "hand").asserting { hand =>
      (effectRowReturn(hand), effectRowParameters(hand)) shouldBe (Seq.empty, Seq.empty)
    }
  }

  // --- effects-as-channel §7 step 3: the carrier-stack recognition tag (finding 14). A *pinned* row `{X[E] | G} A`
  // desugars to the canonical carrier stack `XCarrier[E, G, A]`; the position is marked on `effectRow.returnPinned` /
  // `pinnedParameterIndices` so the checker can later split it as a carrier slot without re-deriving carrier-ness
  // from shape or name. Since effects-as-rows R2 the *entries* are recorded too (`pinnedParameterEffects` /
  // `returnPinnedEffects`, in declared = discharge order), so the entry's ability name resolves like an open-row
  // entry's — the ability must be declared (in real code the `<Ability>Carrier` is colocated with its ability, so one
  // resolves iff the other does). ---

  "the pinned-row recognition tag" should "record a discharger-style pinned parameter with its entries (catch's `{Throw[E] | G} A` shape)" in {
    val source = "data Str\ndata XCarrier[E, G, A]\nability X[E, F[_]] { def op(v: E): F[Str] }\n" +
      "def discharge[E, G, A](obj: {X[E] | G} A): Str"
    runEngineForResolvedValue(source, "discharge").asserting { d =>
      (effectRowReturnPinned(d), effectRowPinnedParamEntries(d)) shouldBe (false, Seq((0, Seq("X"))))
    }
  }

  it should "record a pinned-row return with its entries (a value whose type is a carrier stack)" in {
    val source = "data Str\ndata XCarrier[E, G, A]\nability X[E, F[_]] { def op(v: E): F[Str] }\n" +
      "def make[E, G, A]: {X[E] | G} A"
    runEngineForResolvedValue(source, "make").asserting { m =>
      (effectRowReturnPinnedEntries(m), effectRowPinnedParams(m)) shouldBe (Seq("X"), Set.empty)
    }
  }

  it should "record a multi-entry pinned stack's entries in declared (discharge) order" in {
    val source = "data Str\ndata XCarrier[G, A]\ndata YCarrier[G, A]\n" +
      "ability X[F[_]] { def opX: F[Str] }\nability Y[F[_]] { def opY: F[Str] }\n" +
      "def dischargeBoth[G, A](obj: {X, Y | G} A): Str"
    runEngineForResolvedValue(source, "dischargeBoth").asserting { d =>
      effectRowPinnedParamEntries(d) shouldBe Seq((0, Seq("X", "Y")))
    }
  }

  // A pinned row **stored in a `data` field** must tag the functions the field is split into: the value constructor's
  // parameter and the field accessor's return. Both are the same capture position seen from the two sides, and both
  // used to come out untagged, because `CoreProcessor` ran `EffectSugarDesugarer.desugar(DataDefinition)` — which
  // collapsed the row to its carrier stack — *before* `DataDefinitionDesugarer` built those functions, leaving the
  // per-function pass nothing to read. The row elaborator then hoisted a stored computation onto the ambient carrier
  // instead of capturing it (docs/effects-as-rows.md §9 item 4). The data-level pass now rewrites only *open* rows.
  it should "record a pinned `data` field on the value constructor it is split into" in {
    val source = "data Str\ndata XCarrier[E, G, A]\nability X[E, F[_]] { def op(v: E): F[Str] }\n" +
      "data Holder[E, G](tag: Str, run: {X[E] | G} Str)"
    runEngineForResolvedValue(source, "Holder").asserting { holder =>
      (effectRowReturnPinned(holder), effectRowPinnedParamEntries(holder)) shouldBe (false, Seq((1, Seq("X"))))
    }
  }

  it should "record the same pinned `data` field on the accessor's return" in {
    val source = "data Str\ndata XCarrier[E, G, A]\nability X[E, F[_]] { def op(v: E): F[Str] }\n" +
      "data Holder[E, G](run: {X[E] | G} Str)"
    runEngineForResolvedValue(source, "run").asserting { accessor =>
      (effectRowReturnPinnedEntries(accessor), effectRowPinnedParams(accessor)) shouldBe (Seq("X"), Set.empty)
    }
  }

  it should "NOT mark a data-type parameter or an open-row parameter (only the open row feeds the entries)" in {
    // `box: Box[Str]` is a plain data type; `eff: {Susp} Str` is an OPEN row. Neither is a pinned carrier stack, so the
    // pinned tag stays empty — while the open row still populates `parameterEffects` at its index, showing the two
    // channels are disjoint.
    val source =
      "data Str\ndata Box[X]\nability Susp[F[_]] { def d(v: Str): F[Str] }\n" +
        "def f(box: Box[Str], eff: {Susp} Str): Str"
    runEngineForResolvedValue(source, "f").asserting { f =>
      (effectRowReturnPinned(f), effectRowPinnedParams(f), effectRowParameters(f)) shouldBe
        (false, Set.empty, Seq((1, Seq("Susp"))))
    }
  }

  private def effectRowReturn(rv: OperatorResolvedValue): Seq[String] =
    rv.effectRow.returnEffects.map(_.abilityFQN.abilityName)

  private def effectRowParameters(rv: OperatorResolvedValue): Seq[(Int, Seq[String])] =
    rv.effectRow.parameterEffects.map(pe => (pe.parameterIndex, pe.effects.map(_.abilityFQN.abilityName)))

  private def effectRowReturnPinned(rv: OperatorResolvedValue): Boolean =
    rv.effectRow.returnPinned

  private def effectRowPinnedParams(rv: OperatorResolvedValue): Set[Int] =
    rv.effectRow.pinnedParameterIndices

  private def effectRowReturnPinnedEntries(rv: OperatorResolvedValue): Seq[String] =
    rv.effectRow.returnPinnedEffects.map(_.abilityFQN.abilityName)

  private def effectRowPinnedParamEntries(rv: OperatorResolvedValue): Seq[(Int, Seq[String])] =
    rv.effectRow.pinnedParameterEffects.map(pe => (pe.parameterIndex, pe.effects.map(_.abilityFQN.abilityName)))

  private def signatureShow(rv: OperatorResolvedValue): String =
    rv.signature.value.render

  private def constraintShow(rv: OperatorResolvedValue): Map[String, Seq[(String, Seq[String])]] =
    rv.paramConstraints.view
      .mapValues(_.map(c => (c.abilityFQN.abilityName, c.typeArgs.map(_.render))))
      .toMap

  private def runEngineForResolvedValue(source: String, name: String): IO[OperatorResolvedValue] =
    runGenerator(
      source,
      OperatorResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))),
      systemImports
    ).map { case (errors, facts) =>
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      facts.values
        .collectFirst {
          case rv: OperatorResolvedValue if rv.vfqn.name == QualifiedName(name, Qualifier.Default) => rv
        }
        .getOrElse(throw new Exception(s"No OperatorResolvedValue for $name"))
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

  private def runEngineForErrors(source: String): IO[Seq[TestError]] =
    runGenerator(
      source,
      OperatorResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("main", Qualifier.Default))),
      systemImports
    ).map(result => toTestErrors(result._1))
}
