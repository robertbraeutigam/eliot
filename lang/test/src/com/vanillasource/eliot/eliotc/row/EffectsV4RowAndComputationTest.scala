package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CanonicalRow, GroundValue, GroundValueRenderer}
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, Unifier}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** Effects-as-channel v4, phase P1 (`docs/effects-as-channel-v4.md` §11): the row and the computation type exist in the
  * type language, with one spelling per row, ordinary definitional equality, and a fixed row ⤳ carrier-stack rule.
  *
  * Nothing produces either former yet, so these are unit tests over the domain itself. They pin the three obligations
  * P1 is gated on:
  *
  *   - **one spelling per row** (§4, §9 rule 2) — canonical order and deduplication, and a read-back that canonicalises
  *     unconditionally so no producer can mint a second spelling;
  *   - **definitional equality, no inference** (§9 rule 1) — rows unify structurally as sets, and no row metavariable is
  *     ever created;
  *   - **the canonical stack** (§6, `docs/effects-v4-p0-spike.md` S3) — the rule a *stored* computation's
  *     representation is computed by, since a v4 computation type carries no base to read one off.
  */
class EffectsV4RowAndComputationTest extends AnyFlatSpec with Matchers {
  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  private def abilityFQN(module: String): ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "effect"), module), QualifiedName(module, Qualifier.Type))

  private def typeFQN(packages: Seq[String], module: String, name: String): ValueFQN =
    ValueFQN(ModuleName(packages, module), QualifiedName(name, Qualifier.Type))

  private val console = abilityFQN("Console")
  private val state   = abilityFQN("State")
  private val throwing = abilityFQN("Throw")

  private def con(fqn: ValueFQN, args: GroundValue*): GroundValue = GroundValue.Structure(fqn, args, GroundValue.Type)

  private val string = con(WellKnownTypes.stringFQN)
  private val bigInt = con(WellKnownTypes.bigIntFQN)
  private val unit   = con(typeFQN(Seq("eliot", "lang"), "Unit", "Unit"))
  private val id     = con(WellKnownTypes.idFQN)
  private val io     = con(typeFQN(Seq("eliot", "jvm"), "IO", "IO"))

  private def entry(ability: ValueFQN, args: GroundValue*): GroundValue.Row.Entry =
    GroundValue.Row.Entry(ability, args)

  private def semEntry(ability: ValueFQN, args: GroundValue*): VRow.Entry =
    VRow.Entry(ability, args.map(VConst.apply))

  private def unifier: Unifier = Unifier.create(MetaStore.empty, 0)

  // --- one spelling per row -------------------------------------------------------------------------------------

  "a row" should "canonicalise to one spelling regardless of the order it was written in" in {
    CanonicalRow.row(Seq(entry(state, bigInt), entry(console))) shouldBe
      CanonicalRow.row(Seq(entry(console), entry(state, bigInt)))
  }

  it should "deduplicate an ability repeated at exactly the same arguments" in {
    CanonicalRow.row(Seq(entry(console), entry(console))).entries shouldBe Seq(entry(console))
  }

  it should "keep one ability occurring at two different arguments as two entries" in {
    CanonicalRow.row(Seq(entry(state, string), entry(state, bigInt))).entries.size shouldBe 2
  }

  it should "be idempotent under canonicalisation" in {
    val once = CanonicalRow.canonicalise(Seq(entry(state, bigInt), entry(console), entry(console)))
    CanonicalRow.canonicalise(once) shouldBe once
  }

  "reading a row back" should "canonicalise it, so no producer can mint a second spelling" in {
    Quoter.quote(0, VRow(Seq(semEntry(state, bigInt), semEntry(console))), MetaStore.empty) shouldBe
      Right(CanonicalRow.row(Seq(entry(console), entry(state, bigInt))))
  }

  "the row operations" should "union canonically" in {
    CanonicalRow.union(CanonicalRow.row(Seq(entry(console))), CanonicalRow.row(Seq(entry(console), entry(state, bigInt)))) shouldBe
      CanonicalRow.row(Seq(entry(console), entry(state, bigInt)))
  }

  it should "subtract a discharged entry" in {
    CanonicalRow.difference(
      CanonicalRow.row(Seq(entry(console), entry(throwing, string))),
      CanonicalRow.row(Seq(entry(throwing, string)))
    ) shouldBe CanonicalRow.row(Seq(entry(console)))
  }

  it should "decide subset inclusion, which is what `derived ⊆ declared` needs" in {
    CanonicalRow.subsetOf(CanonicalRow.row(Seq(entry(console))), CanonicalRow.row(Seq(entry(console), entry(state, bigInt)))) shouldBe true
  }

  // --- definitional equality, no inference ----------------------------------------------------------------------

  "two rows written in different orders" should "unify — a row is a set, not a sequence" in {
    unifier.unify(VRow(Seq(semEntry(console), semEntry(state, bigInt))), VRow(Seq(semEntry(state, bigInt), semEntry(console))), ctx).errors shouldBe empty
  }

  "two rows naming the same ability at different arguments" should "fail to unify" in {
    unifier.unify(VRow(Seq(semEntry(state, bigInt))), VRow(Seq(semEntry(state, string))), ctx).errors should not be empty
  }

  "rows of different sizes" should "fail to unify" in {
    unifier.unify(VRow(Seq(semEntry(console))), VRow(Seq(semEntry(console), semEntry(state, bigInt))), ctx).errors should not be empty
  }

  "two computation types" should "unify when their rows and payloads agree" in {
    unifier.unify(VComputation(VRow(Seq(semEntry(console))), VConst(unit)), VComputation(VRow(Seq(semEntry(console))), VConst(unit)), ctx).errors shouldBe empty
  }

  it should "fail to unify when their payloads differ" in {
    unifier.unify(VComputation(VRow(Seq(semEntry(console))), VConst(unit)), VComputation(VRow(Seq(semEntry(console))), VConst(string)), ctx).errors should not be empty
  }

  "a computation type meeting a plain payload type" should "be an ordinary mismatch — rule 4 as a theorem, not an elaborator arm" in {
    unifier.unify(VComputation(VRow(Seq(semEntry(console))), VConst(string)), VConst(string), ctx).errors should not be empty
  }

  "unifying rows" should "never create a row metavariable" in {
    unifier.unify(VRow(Seq(semEntry(console))), VRow(Seq(semEntry(console))), ctx).metaStore shouldBe MetaStore.empty
  }

  // --- rendering ------------------------------------------------------------------------------------------------

  "a computation type" should "render in row-and-payload vocabulary, with no carrier name" in {
    GroundValueRenderer.render(GroundValue.Computation(CanonicalRow.row(Seq(entry(console))), unit)) shouldBe "{Console} Unit"
  }

  it should "render a multi-entry row in canonical order" in {
    GroundValueRenderer.render(
      GroundValue.Computation(CanonicalRow.row(Seq(entry(state, bigInt), entry(console))), string)
    ) shouldBe "{Console, State[BigInteger]} String"
  }

  "the empty row" should "render as the empty brace, which is a legal spelling of the pure row" in {
    GroundValueRenderer.render(GroundValue.Row.empty) shouldBe "{}"
  }

  "a semantic computation type" should "print the same way as its ground counterpart" in {
    SemValuePrinter.show(VComputation(VRow(Seq(semEntry(console))), VConst(unit)), MetaStore.empty) shouldBe "{Console} Unit"
  }

  // --- the canonical row ⤳ stack rule ---------------------------------------------------------------------------

  /** `Console` has no carrier of its own — it rides `Suspend` — so it lowers onto the platform run carrier. */
  private def carrierOf(ability: ValueFQN): Option[ValueFQN] =
    Option.when(ability != console)(typeFQN(Seq("eliot", "effect"), ability.name.name, ability.name.name + "Carrier"))

  private def stackOf(row: GroundValue.Row): CanonicalStack.Stack = CanonicalStack.of(row, carrierOf, id, io)

  "the empty row" should "lower to nothing at all — pure code lowers to itself" in {
    stackOf(GroundValue.Row.empty).representation(string) shouldBe string
  }

  "a Suspend-free row" should "lower to its carriers over the pure base" in {
    stackOf(CanonicalRow.row(Seq(entry(throwing, string)))).representation(unit) shouldBe
      con(typeFQN(Seq("eliot", "effect"), "Throw", "ThrowCarrier"), string, id, unit)
  }

  "a Suspend-riding row" should "lower over the platform run carrier" in {
    stackOf(CanonicalRow.row(Seq(entry(console)))).representation(unit) shouldBe
      con(typeFQN(Seq("eliot", "jvm"), "IO", "IO"), unit)
  }

  it should "lower its dischargeable entries over the run carrier" in {
    stackOf(CanonicalRow.row(Seq(entry(console), entry(throwing, string)))).representation(unit) shouldBe
      con(typeFQN(Seq("eliot", "effect"), "Throw", "ThrowCarrier"), string, io, unit)
  }

  "the canonical order" should "fix a stored computation's transformer nesting (R8), leftmost entry outermost" in {
    stackOf(CanonicalRow.row(Seq(entry(state, bigInt), entry(throwing, string)))).layers.map(_.carrier.name.name) shouldBe
      Seq("StateCarrier", "ThrowCarrier")
  }

  "the same row written two ways" should "lower to the same stack" in {
    stackOf(CanonicalRow.row(Seq(entry(throwing, string), entry(state, bigInt)))) shouldBe
      stackOf(CanonicalRow.row(Seq(entry(state, bigInt), entry(throwing, string))))
  }
}
