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

/** Pins the representation-reconcile pass's placement policy (`docs/bounds-as-refinements.md`): given a hand-built
  * uncurried body and a channel [[RefinementTable]], it must stamp the right node metas and insert
  * [[ReconciledMonomorphicExpression.Reconcile]] nodes at exactly the meta-change edges — branch-arm merges and
  * argument/return boundaries — while leaving the width-transparent arithmetic/ordering/`toString` operands alone.
  *
  * Only [[ReconcileProcessor]] runs (both inputs are injected as facts), so these tests exercise the placement logic in
  * isolation — no stdlib, no channel, no codegen — which is the whole point of making the conversion an explicit,
  * testable node rather than scattered backend bytecode.
  */
class ReconcileProcessorTest extends ProcessorTest(ReconcileProcessor()) {

  "return boundary" should "wrap a returned narrow literal in a reconcile to ⊤ (None)" in {
    reconciledTop(uInt(1, 300), table(iv(1, 300, 300))).asserting(_.value.meta shouldBe None)
  }

  it should "keep the literal's own meta on the reconcile source" in {
    reconciledTop(uInt(1, 300), table(iv(1, 300, 300)))
      .asserting(reconcileSource(_).value.meta shouldBe Some(ReconcileProcessor.intervalMeta(300, 300)))
  }

  it should "not wrap a returned ⊤ value (no channel interval)" in {
    reconciledTop(uParam(1, "x", intType), table()).asserting(isReconcile(_) shouldBe false)
  }

  "arithmetic leaf" should "not reconcile its operands (width-transparent)" in {
    reconciledTop(addBody, addTable)
      .asserting(top => appArgs(reconcileSource(top)).map(isReconcile) shouldBe Seq(false, false))
  }

  it should "keep each operand's own narrow meta" in {
    reconciledTop(addBody, addTable)
      .asserting(top =>
        appArgs(reconcileSource(top)).map(_.value.meta) shouldBe
          Seq(Some(ReconcileProcessor.intervalMeta(1, 1)), Some(ReconcileProcessor.intervalMeta(300, 300)))
      )
  }

  it should "stamp the transfer result meta on the application node" in {
    reconciledTop(addBody, addTable)
      .asserting(reconcileSource(_).value.meta shouldBe Some(ReconcileProcessor.intervalMeta(301, 301)))
  }

  "ordering leaf intLessThanOrEqual" should "not reconcile its operands (width-transparent)" in {
    val body = uApp(1, boolType, uRef(2, langInt("intLessThanOrEqual")), uInt(3, 1), uInt(4, 300))
    reconciledTop(body, table(iv(3, 1, 1), iv(4, 300, 300)), returnType = boolType)
      .asserting(appArgs(_).map(isReconcile) shouldBe Seq(false, false))
  }

  "branch merge (Bool::fold)" should "reconcile each arm to the fold node's joined meta" in {
    reconciledTop(foldBody, foldTable)
      .asserting(top => appArgs(reconcileSource(top)).drop(1).map(isReconcile) shouldBe Seq(true, true))
  }

  it should "target the arm reconciles at the join meta" in {
    reconciledTop(foldBody, foldTable)
      .asserting(top =>
        appArgs(reconcileSource(top)).drop(1).map(_.value.meta) shouldBe
          Seq(Some(ReconcileProcessor.intervalMeta(1, 300)), Some(ReconcileProcessor.intervalMeta(1, 300)))
      )
  }

  it should "keep each arm's own narrow meta under its reconcile" in {
    reconciledTop(foldBody, foldTable)
      .asserting(top =>
        appArgs(reconcileSource(top)).drop(1).map(a => reconcileSource(a).value.meta) shouldBe
          Seq(Some(ReconcileProcessor.intervalMeta(1, 1)), Some(ReconcileProcessor.intervalMeta(300, 300)))
      )
  }

  it should "not reconcile the condition" in {
    reconciledTop(foldBody, foldTable).asserting(top => isReconcile(appArgs(reconcileSource(top)).head) shouldBe false)
  }

  "ordinary call boundary" should "reconcile a narrow Int argument to ⊤ (None)" in {
    reconciledTop(callBody, callTable, returnType = stringType).asserting(top => appArgs(top).head.value.meta shouldBe None)
  }

  it should "keep the argument's own meta under the reconcile" in {
    reconciledTop(callBody, callTable, returnType = stringType)
      .asserting(top => reconcileSource(appArgs(top).head).value.meta shouldBe Some(ReconcileProcessor.intervalMeta(300, 300)))
  }

  it should "not reconcile a non-Int (String) argument" in {
    reconciledTop(callBody, callTable, returnType = stringType).asserting(top => isReconcile(appArgs(top)(1)) shouldBe false)
  }

  "metaByPosition" should "drop a position carrying two distinct intervals (ambiguous)" in {
    ReconcileProcessor.metaByPosition(table(iv(1, 0, 5), iv(1, 0, 9))).get(pos(1)) shouldBe None
  }

  it should "keep a position whose entries all agree" in {
    ReconcileProcessor.metaByPosition(table(iv(1, 0, 5), iv(1, 0, 5))).get(pos(1)) shouldBe
      Some(ReconcileProcessor.intervalMeta(0, 5))
  }

  "widthTransparentLeaves" should "be exactly the arithmetic/ordering/toString leaves" in {
    ReconcileProcessor.widthTransparentLeaves shouldBe
      Set("nativeAdd", "nativeSubtract", "nativeMultiply", "intLessThanOrEqual", "intToString").map(langInt)
  }

  "boolFoldFqn" should "name the Bool fold eliminator" in {
    ReconcileProcessor.boolFoldFqn shouldBe
      ValueFQN(ModuleName(defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Default))
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

  private val intType: GroundValue    = structureType("Int", "Int")
  private val stringType: GroundValue = structureType("String", "String")
  private val boolType: GroundValue   = structureType("Bool", "Bool")

  private val mainFqn: ValueFQN = ValueFQN(testModuleName, QualifiedName("main", Qualifier.Default))

  private def pos(n: Int): PositionRange       = PositionRange(Position(1, n), Position(1, n + 1))
  private def at[T](n: Int, v: T): Sourced[T]  = Sourced(file, pos(n), v)

  private def uInt(n: Int, v: Int): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(intType, U.IntegerLiteral(at(n, BigInt(v)))))
  private def uStr(n: Int, s: String): Sourced[UncurriedMonomorphicExpression] =
    at(n, U(stringType, U.StringLiteral(at(n, s))))
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

  private def isReconcile(n: Sourced[ReconciledMonomorphicExpression]): Boolean =
    n.value.expression.isInstanceOf[ReconciledMonomorphicExpression.Reconcile]

  private def reconcileSource(n: Sourced[ReconciledMonomorphicExpression]): Sourced[ReconciledMonomorphicExpression] =
    n.value.expression match {
      case ReconciledMonomorphicExpression.Reconcile(s) => s
      case other                                        => fail(s"expected a Reconcile node, got: $other")
    }

  private def appArgs(n: Sourced[ReconciledMonomorphicExpression]): Seq[Sourced[ReconciledMonomorphicExpression]] =
    n.value.expression match {
      case ReconciledMonomorphicExpression.FunctionApplication(_, args) => args
      case other                                                        => fail(s"expected a FunctionApplication node, got: $other")
    }

  private val addBody: Sourced[UncurriedMonomorphicExpression] =
    uApp(1, intType, uRef(2, langInt("nativeAdd")), uInt(3, 1), uInt(4, 300))
  private val addTable: RefinementTable = table(iv(1, 301, 301), iv(3, 1, 1), iv(4, 300, 300))

  private val foldBody: Sourced[UncurriedMonomorphicExpression] =
    uApp(1, intType, uRef(2, ReconcileProcessor.boolFoldFqn), uParam(3, "c", boolType), uInt(4, 1), uInt(5, 300))
  private val foldTable: RefinementTable = table(iv(1, 1, 300), iv(4, 1, 1), iv(5, 300, 300))

  private val callBody: Sourced[UncurriedMonomorphicExpression] =
    uApp(
      1,
      stringType,
      uRef(2, ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))),
      uInt(3, 300),
      uStr(4, "x")
    )
  private val callTable: RefinementTable = table(iv(3, 300, 300))
}
