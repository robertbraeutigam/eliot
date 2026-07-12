package com.vanillasource.eliot.eliotc.reconcile.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.reconcile.fact.{ReconciledMonomorphicExpression, ReconciledMonomorphicValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression as U
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue

/** Pins the representation-reconcile pass, now a pure **meta-stamper**: given a hand-built uncurried body and a channel
  * [[RefinementTable]], it stamps each node with the channel's interval (or [[None]] for a ⊤ node) and preserves the
  * structure verbatim — it inserts no conversion nodes and names no construct. The backend derives every width and
  * re-encode from these stamped metas (`docs/generic-refinement-merges.md`).
  *
  * Only [[ReconcileProcessor]] runs (both inputs are injected as facts), so these tests exercise the stamping in
  * isolation — no stdlib, no channel, no codegen.
  */
class ReconcileProcessorTest extends ProcessorTest(ReconcileProcessor()) {

  "meta stamping" should "stamp a node's channel interval as its meta" in {
    reconciledTop(uInt(1, 300), table(iv(1, 300, 300)))
      .asserting(_.value.meta shouldBe Some(ReconcileProcessor.intervalMeta(300, 300)))
  }

  it should "stamp None for a ⊤ node (no channel interval)" in {
    reconciledTop(uParam(1, "x", intType), table()).asserting(_.value.meta shouldBe None)
  }

  it should "stamp the transfer result meta on an arithmetic application node" in {
    reconciledTop(addBody, addTable).asserting(_.value.meta shouldBe Some(ReconcileProcessor.intervalMeta(301, 301)))
  }

  it should "keep each arithmetic operand's own narrow meta (fast path preserved)" in {
    reconciledTop(addBody, addTable)
      .asserting(top =>
        appArgs(top).map(_.value.meta) shouldBe
          Seq(Some(ReconcileProcessor.intervalMeta(1, 1)), Some(ReconcileProcessor.intervalMeta(300, 300)))
      )
  }

  it should "preserve application structure verbatim (no wrapping)" in {
    reconciledTop(addBody, addTable).asserting(appArgs(_).size shouldBe 2)
  }

  "metaByPosition" should "drop a position carrying two distinct intervals (ambiguous)" in {
    ReconcileProcessor.metaByPosition(Seq(iv(1, 0, 5), iv(1, 0, 9))).get(pos(1)) shouldBe None
  }

  it should "keep a position whose entries all agree" in {
    ReconcileProcessor.metaByPosition(Seq(iv(1, 0, 5), iv(1, 0, 5))).get(pos(1)) shouldBe
      Some(ReconcileProcessor.intervalMeta(0, 5))
  }

  // ---- fixtures -----------------------------------------------------------------------------------------------------

  private def langInt(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  private def structureType(module: String, name: String): GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, module), QualifiedName(name, Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val intType: GroundValue = structureType("Int", "Int")

  private val mainFqn: ValueFQN = ValueFQN(testModuleName, QualifiedName("main", Qualifier.Default))

  private def pos(n: Int): PositionRange      = PositionRange(Position(1, n), Position(1, n + 1))
  private def at[T](n: Int, v: T): Sourced[T] = Sourced(file, pos(n), v)

  private def uInt(n: Int, v: Int): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(intType, U.IntegerLiteral(at(n, BigInt(v)))))
  private def uRef(n: Int, fqn: ValueFQN): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(intType, U.MonomorphicValueReference(at(n, fqn), Seq.empty)))
  private def uParam(n: Int, name: String, tpe: GroundValue): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(tpe, U.ParameterReference(at(n, name))))
  private def uApp(
      n: Int,
      tpe: GroundValue,
      target: Sourced[UncurriedMonomorphicExpression],
      args: Sourced[UncurriedMonomorphicExpression]*
  ): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(tpe, U.FunctionApplication(target, args)))

  private def iv(p: Int, min: Int, max: Int): RefinementTable.NodeInterval =
    RefinementTable.NodeInterval(pos(p), BigInt(min), BigInt(max))
  private def table(intervals: RefinementTable.NodeInterval*): RefinementTable =
    RefinementTable(mainFqn, Seq.empty, intervals)

  private def umv(body: Sourced[UncurriedMonomorphicExpression], returnType: GroundValue): UncurriedMonomorphicValue =
    UncurriedMonomorphicValue(
      vfqn = mainFqn,
      typeArguments = Seq.empty,
      arity = 0,
      name = at(0, QualifiedName("main", Qualifier.Default)),
      signature = returnType,
      parameters = Seq.empty,
      returnType = returnType,
      body = Some(body.map(_.expression))
    )

  private def reconciledTop(
      body: Sourced[UncurriedMonomorphicExpression],
      tbl: RefinementTable,
      returnType: GroundValue = intType
  ): IO[Sourced[ReconciledMonomorphicExpression]] =
    runGeneratorWithFacts(Seq(umv(body, returnType), tbl), ReconciledMonomorphicValue.Key(mainFqn, Seq.empty, 0))
      .map { case (opt, _) => opt.get.body.get }

  private def appArgs(n: Sourced[ReconciledMonomorphicExpression]): Seq[Sourced[ReconciledMonomorphicExpression]] =
    n.value.expression match {
      case ReconciledMonomorphicExpression.FunctionApplication(_, args) => args
      case other                                                        => fail(s"expected a FunctionApplication node, got: $other")
    }

  private val addBody: Sourced[UncurriedMonomorphicExpression] =
    uApp(1, intType, uRef(2, langInt("nativeAdd")), uInt(3, 1), uInt(4, 300))
  private val addTable: RefinementTable = table(iv(1, 301, 301), iv(3, 1, 1), iv(4, 300, 300))
}
