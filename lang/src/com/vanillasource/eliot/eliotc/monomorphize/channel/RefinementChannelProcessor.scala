package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.processor.{MetaConstructorDesugarer, MetaWhereDesugarer}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Literal
import com.vanillasource.eliot.eliotc.monomorphize.processor.EscalatingReducer
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The refinement channel's **flow analysis** — Step 6-iii of `docs/bounds-as-refinements.md` ("narrow representations
  * from the channel's flow analysis"). Post-flag-day (Step 6-ii) `Int` has lost its type parameters, so a node's value
  * range is no longer in its type; the channel *computes* it by flow and records it into a [[RefinementTable]], keyed by
  * source position. The reconcile pass ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]]) then
  * stamps those intervals onto the body as per-node metas, and the JVM backend decodes each `Int`'s machine layout from
  * its meta (a narrow wrapper instead of the ⊤/bignum fallback).
  *
  * A post-pass over each [[MonomorphicValue]] (runtime track): it walks the fully-ground body bottom-up and, for every
  * node whose value range it can pin, records that interval. The propagation rules (the value channel of §4):
  *
  *   - **α (literal seeding):** an integer literal `n` seeds the singleton `[n, n]`.
  *   - **Transfers (Step-4c form):** at an `Int` `+`/`-`/`*` leaf the result interval is the leaf's `^Meta` transfer
  *     companion (`add^Meta`/… — the `Numeric[Int]` instance methods' companions, whose braces spell the transfer as
  *     `add`/`subtract`/`multiply` over the operand ranges, dispatched through the `Numeric[Interval[BigInteger]]`
  *     instance and bottoming at `Numeric[BigInteger]` natives) evaluated through the one NbE evaluator on the two
  *     operand intervals. Unknown if either operand is unknown.
  *   - **Merges (Step 3):** at *any ordinary call* whose callee declares a `^Meta` **merge** companion — e.g. `fold`,
  *     whose `fold^Meta` spells `join(whenTrue, whenFalse)` over the domain's `Meta.join` — the result interval is that
  *     companion reduced on the argument metas (`mergeViaCompanion`), *mechanically identical* to a transfer: no branch
  *     construct is ever named. So `fold` narrows through the same generic path as any range-moving native, and any
  *     future selector merges for free. The arms keep their own (narrower) intervals; the reconcile pass re-encodes
  *     each to the merged representation at the branch (`docs/generic-refinement-merges.md` §1).
  *
  * Everything else is ⊤ (unknown, laid out as a bignum) — a parameter, a value reference, a `match` (`handleCases`)
  * result, the body of a lambda, an ordinary call with no `^Meta` companion. These are the boundaries of §4/§7 Q4: the
  * flow analysis is intra-procedural, so a value crossing a call/return/field/lambda boundary is ⊤ there (sound: "I know
  * nothing" is always true, just imprecise). A ⊤ node is still *recorded*, as a `None` verdict — see [[recordAt]] for
  * why omitting it is unsound at an aliased position.
  *
  * The walk **descends everywhere**: into the arguments of ordinary calls (so a literal/arithmetic argument narrows and
  * is reconciled to the callee's parameter representation at the call), into a lambda's body, and into the head of an
  * application the channel cannot recognise (an applied lambda — what a pure `val` block lowers to). Descending is what
  * makes the use-site `where` demand total; *recording* is the narrower privilege, and it stops at a lambda (a narrow
  * value returned through a lambda's `apply` bridge would fail its `CHECKCAST` — see the class note on
  * `LambdaGenerator`), which is why those two subtrees are walked for their checks and their records discarded.
  *
  * Why a post-pass and not a rider inside the checker: refinements are, by the design's held invariant, strictly
  * *downstream* of type formation (they flow into checks and codegen, never back into a type), so the channel can run
  * entirely over the checker's output with zero risk to the checker's invariants. See the design doc §3.
  *
  * A transfer and a merge are recognised the *same* way: solely by the callee declaring a `^Meta` companion
  * (`metaCompanionFqn`), never by naming a native leaf or a branch construct (`docs/generic-refinement-merges.md`).
  * The companion's body may itself route through ability instances (a transfer's `Numeric[Interval]`, a merge's
  * `Meta.join`); the post-monomorphize linker-executor ([[EscalatingReducer]]) resolves those through each instance's
  * own monomorphization, so the former "a transfer must bottom out at natives" restriction is repealed
  * (`docs/refinement-channel-follow-ups.md`). A callee with no `^Meta` companion simply gets no narrowing
  * there — a bignum layout, sound but wide, never wrong.
  */
class RefinementChannelProcessor
    extends TransformationProcessor[MonomorphicValue.Key, RefinementTable.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  import RefinementChannelProcessor.*

  /** The result of walking one node: the opaque meta [[GroundValue]] the channel knows for the node's *own* value
    * (⊤ = [[None]]) and every per-node verdict recorded in the subtree (this node's plus its descendants').
    */
  private case class Flow(
      own: Option[GroundValue],
      records: Seq[RefinementTable.NodeMeta]
  )

  override protected def generateFromKeyAndFact(
      key: RefinementTable.Key,
      mv: MonomorphicValue
  ): CompilerIO[RefinementTable] = {
    // Effects-as-channel §6/§10: the flow analysis reads `MonomorphicValue`, a **sibling** of `WovenValueProcessor` —
    // so under the uniform-carrier path its body still carries the pervasive `Id` machinery (`pure@Effect[Id]`/`runId`
    // wrappers, `Id[X]` node/signature types) that the codegen seam erases but this channel would otherwise trip over: a
    // literal's `[n,n]` range hides inside `pure@Effect[Id]( n )` and a merge's `A := Id[Int]` finds no `Id$Meta`, so a
    // provable range reads as ⊤ ("value range is not known"). Normalize `Id` away up front, exactly as
    // `WovenValueProcessor` does (a no-op on the legacy path, which inserts no `eliot.lang.Id`), so the flow sees the bare
    // literal and `Id`-erased types.
    val erasedSig      = IdNormalizer.eraseIdTypes(mv.signature)
    val normalizedBody =
      mv.runtime.map(body => IdNormalizer.eraseIdInBody(IdNormalizer.normalizeValue(mv.vfqn, mv.signature, body)))
    for {
      result <- normalizedBody match {
                  case Some(body) => walkFlow(body.as(MonomorphicExpression(erasedSig, body.value)))
                  case None       => Flow(None, Seq.empty).pure[CompilerIO]
                }
    } yield RefinementTable(key.vfqn, key.typeArguments, result.records)
  }

  /** Compute one node's flow interval and record it (when known), descending per the propagation rules in the class
    * note. Bottom-up: a node's interval is derived from its children's, and a known interval is recorded at the node's
    * source position for representation lowering to read.
    */
  private def walkFlow(node: Sourced[MonomorphicExpression]): CompilerIO[Flow] =
    node.value.expression match {
      case MonomorphicExpression.IntegerLiteral(value)  =>
        // α (the one point a meta *originates*): a literal `n` seeds its singleton range by reducing the literal
        // protocol's own `^Meta` companion on `n`. The seed's construction (`Int$Meta(Bounded(Interval[n, n]))`) lives
        // in Eliot — `eliot.lang.Runtime::integerLiteral`'s return brace `{Bounded(Interval(V, V))}`, which the desugarer turns into
        // `integerLiteral^Meta` — and is reduced here through the *same* uniform `^Meta` path as every transfer/merge
        // (`metaViaCompanion`). The channel builds no domain structure of its own; it only wraps `n` as its
        // `BigInteger` value to reduce the companion at. So even the α origin is domain-agnostic.
        metaViaCompanion(
          WellKnownTypes.integerLiteralFQN,
          Seq(GroundValue.Direct(value.value, bigIntType)),
          Seq.empty
        ).map(meta => Flow(meta, recordAt(node, meta)))

      case _: MonomorphicExpression.FunctionApplication =>
        val (head, args) = flatten(node)
        head.value.expression match {
          case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
            // An ordinary call (or constructor). Descend into the arguments (so a literal/arithmetic argument narrows,
            // and a `where` precondition is demanded over their ranges, bounds-as-refinements §4.3). The result is a ⊤
            // boundary **unless** the callee declares a `^Meta` companion — a **transfer** (`Numeric[Int]::add`, whose
            // result range is `Numeric[Interval]::add` of the operand ranges) or a **merge** (`fold`, whose result range is
            // `Meta.join` of its arms). Both are computed by the *same* uniform path: reduce `<callee>^Meta` on the argument metas
            // (`metaViaCompanion`). The channel names no leaf and no branch construct — the `^Meta` companion is the one
            // recognition point (`docs/generic-refinement-merges.md`). A lambda argument's body is skipped by the
            // `FunctionLiteral` case below.
            for {
              argResults <- args.traverse(walkFlow)
              _          <- checkWhere(node, vfqn.value, typeArgs, args.size, argResults.map(_.own))
              merge      <- metaViaCompanion(vfqn.value, typeArgs, argResults.map(_.own))
            } yield Flow(merge, argResults.flatMap(_.records) ++ recordAt(node, merge))
          case _ =>
            // Any other application (a `match`, a `typeMatch`, an applied lambda): the result is a ⊤ boundary; descend
            // into the arguments as above — **and into the head**. The head is where a pure `val` block keeps its
            // continuation (`val x = e ; rest` lowers to `(x -> rest)(e)`, `BlockDesugaringProcessor`), so walking only
            // the arguments checked the *bound* expression and silently skipped everything after the binding: a `where`
            // demand there was never made and a `where`-bearing def referenced there was never rejected — the very hole
            // the [[MonomorphicExpression.FunctionLiteral]] arm below exists to close, reappearing one level up. Its
            // records are discarded on the same grounds as that arm's (this node is a ⊤ boundary either way); only the
            // walk's `checkWhere`/rejection effects remain.
            for {
              _          <- walkFlow(head)
              argResults <- args.traverse(walkFlow)
            } yield Flow(None, argResults.flatMap(_.records) ++ recordAt(node, None))
        }

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        // A lambda body must not *record* narrow intervals: its `apply` bridge would `CHECKCAST` a narrowed result back
        // to the ⊤/bignum representation the caller expects, so it stays a bignum boundary for representation (Step
        // 6-iii). But it must still be *walked*, so a `where` precondition on a call inside it is demanded (a def's own
        // parameters make its body a leading lambda, so without this every call in a parametered def would escape the
        // check — the §4.3 use-site verification must not have that hole). The records are discarded; only `checkWhere`'s
        // effects during the walk remain.
        walkFlow(body).as(Flow(None, recordAt(node, None)))

      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) =>
        // A **bare** reference to a def — *not* the head of a full application (that path is the `FunctionApplication`
        // arm's `checkWhere`). If the def carries a `where` precondition, passing it as a value silently bypasses that
        // precondition: its eventual call rides a function value whose head the channel never sees as a
        // `MonomorphicValueReference`, so the demand is made nowhere (`docs/refinement-channel-follow-ups.md` §2.1).
        // Reject it loudly — the Use-Site Verification cornerstone requires every manifest use to be checked. ⊤ for the
        // node itself (a function value carries no integer range).
        rejectWhereAsValueIfBearing(node, vfqn.value).as(Flow(None, recordAt(node, None)))

      case _ =>
        // A parameter reference or a string literal: ⊤ (no known integer range at this node).
        Flow(None, recordAt(node, None)).pure[CompilerIO]
    }

  /** The refinement result of a call, when its callee declares a `^Meta` companion — a **transfer** (`add^Meta`,
    * whose result range is `Numeric[Interval]::add` of the operand ranges) or a **merge** (`fold^Meta`, whose result
    * range is `Meta.join` of its arms). Both are computed uniformly: reduce `<callee>^Meta` on the arguments' metas and
    * read its result `Int$Meta`'s `range` slot back. `None` (⊤) when the callee has no companion, or an input's range is
    * unknown.
    *
    * The companion is reduced at the **meta** type arguments — the call's base type args mapped through [[metaTypeOf]].
    * A *monomorphic* companion (`add`, no type args) reduces at `[]` and its `Int$Meta` params take the operand
    * metas directly. A *generic* companion (`fold[A]`) reduces at `A := metaTypeOf(Int) = Int$Meta`, so the bare `A`
    * params bind to the meta type; its `join` then dispatches via the compiler-derived `Meta[Int$Meta]` to
    * `Int$Meta(join(range(whenTrue), range(whenFalse)))` — the inner `join` dispatched through the `Meta[Interval]`
    * instance by the channel's post-monomorphize executor. An unknown/non-`Int` argument (⊤ — e.g. `fold`'s
    * `condition`) is a `VType` placeholder the reduction ignores unless it feeds a slot projection, in which case the
    * projection stalls and the result is ⊤ (sound). A merge over untracked arms reduces at `A := Unit` (an untracked
    * type's [[metaTypeOf]] is `Unit`) through the trivial `Meta[Unit]` and reads back no interval (⊤). The membership
    * test (a cached [[UnifiedModuleNames]] lookup for the companion name) keeps a companion-free call to one cheap lookup.
    */
  private def metaViaCompanion(
      callee: ValueFQN,
      calleeTypeArgs: Seq[GroundValue],
      argMetas: Seq[Option[GroundValue]]
  ): CompilerIO[Option[GroundValue]] =
    getFactOrAbort(UnifiedModuleNames.Key(callee.moduleName, Platform.Compiler)).flatMap { names =>
      if (!names.names.contains(QualifiedName(callee.name.name, Qualifier.Meta)))
        none[GroundValue].pure[CompilerIO]
      else
        for {
          metaTypeArgs <- calleeTypeArgs.traverse(metaTypeOf)
          // Reduce `<callee>^Meta` at the meta type args, applied to the argument metas, through the compiler platform's
          // escalating linker-executor (`docs/refinement-channel-follow-ups.md` §1): it links only monomorphized
          // callees, so a transfer/merge whose body routes through an ability instance resolves through that instance's
          // own monomorphization rather than sticking on the abstract ability method. An unknown/untracked argument (⊤)
          // is a `VType` placeholder the reduction ignores unless it feeds a slot projection (then the projection stalls
          // and the result is ⊤, sound). The result meta is an opaque domain structure (the type's `$Meta`), stored
          // verbatim; a stuck/⊤ result does not quote to a structure and is dropped.
          result       <- EscalatingReducer.reduceApplied(
                            metaCompanionFqn(callee),
                            metaTypeArgs,
                            argMetas.map {
                              case Some(gv) => Evaluator.groundToSem(gv) // the argument's own meta value, passed opaquely
                              case None     => SemValue.VType            // ⊤ placeholder: an unknown/untracked argument
                            }
                          )
        } yield result.collect { case s: GroundValue.Structure => s }
    }

  /** The **meta type** of a base type — its `$Meta` meta structure if the type declares one (a slotted type like `Int`
    * ⤳ `Int$Meta`), else the trivial [[unitType]] (any untracked type carries no refinement, so its meta is `Unit`).
    * This is the total-meta rule the deleted `metaOf` intrinsic used to approximate: a generic `^Meta` companion is
    * reduced at these, binding a bare type-parameter param straight to the meta type, and it *always* lands on a real
    * `Meta` instance (`Meta[Int$Meta]` or `Meta[Unit]`) — never a stuck non-existent `T$Meta`. The membership test is a
    * cached [[UnifiedModuleNames]] lookup in the base type's own module. A non-structure type argument is left unchanged.
    */
  private def metaTypeOf(baseType: GroundValue): CompilerIO[GroundValue] = baseType match {
    case GroundValue.Structure(fqn, _, _) =>
      val metaName = QualifiedName(fqn.name.name + MetaConstructorDesugarer.metaTypeSuffix, Qualifier.Type)
      getFactOrAbort(UnifiedModuleNames.Key(fqn.moduleName, Platform.Compiler)).map { names =>
        if (names.names.contains(metaName))
          GroundValue.Structure(ValueFQN(fqn.moduleName, metaName), Seq.empty, GroundValue.Type)
        else unitType
      }
    case other                            => other.pure[CompilerIO]
  }

  /** Record this node's verdict — its pinned meta, or ⊤. Every *walked* node records, ⊤ included: a consumer can only
    * match this table against its own tree by source position, and desugaring makes positions non-unique (a pure `val`
    * block's synthesized lambda and application both carry the bound expression's range), so a ⊤ node that omitted its
    * verdict would let a sibling's meta be read as its own. Recording ⊤ makes such a position ambiguous, which the
    * reconcile pass already drops. See [[RefinementTable.NodeMeta]].
    */
  private def recordAt(
      node: Sourced[MonomorphicExpression],
      meta: Option[GroundValue]
  ): Seq[RefinementTable.NodeMeta] =
    Seq(RefinementTable.NodeMeta(node.range, meta))

  /** Demand a callee's `where` precondition (bounds-as-refinements §4.3) at this call site, when it declares one. A def
    * `def f(x: Int): T where withinByte(range(x))` desugars to a `^Where` companion `f$Where(x: Int$Meta): Bool =
    * withinByte(range(x))` ([[MetaWhereDesugarer]]); at a *full* call to `f` that companion is reduced on the
    * compiler track and evaluated over the arguments' channel intervals. The demand is discharged only when every
    * argument's range is known and the predicate reduces to `true`; an unknown (⊤) argument range or a `false` result is
    * a hard error at the call — the use-site verification the cornerstone prescribes. A partial application is not yet a
    * call (left for the full application); a callee that declares no companion has no `where`. The presence of a
    * companion is a cheap per-module [[UnifiedModuleNames]] membership test, so an ordinary call to a `where`-free callee
    * costs one cached lookup and never demands a non-existent companion fact.
    */
  private def checkWhere(
      callNode: Sourced[MonomorphicExpression],
      callee: ValueFQN,
      calleeTypeArgs: Seq[GroundValue],
      appliedArgs: Int,
      argMetas: Seq[Option[GroundValue]]
  ): CompilerIO[Unit] =
    hasWhereCompanion(callee).flatMap {
      case false => ().pure[CompilerIO]
      case true  =>
        getFactIfProduced(MonomorphicValue.Key(callee, calleeTypeArgs)).flatMap {
          case Some(mv) =>
            mv.naturalArity match {
              case Some(arity) if arity > 0 && appliedArgs >= arity =>
                demandPrecondition(callNode, callee, argMetas.take(arity))
              case Some(arity) if arity > 0                         =>
                // A *partial* application of a `where`-bearing def is a function value with an undemandable precondition
                // — the same escape as a bare reference (§2.1). Reject it loudly rather than let the partial value flow
                // to a call the channel cannot see.
                rejectWhereAsValue(callNode, callee)
              case _                                                => ().pure[CompilerIO]
            }
          case None     => ().pure[CompilerIO]
        }
    }

  /** Whether `callee` declares a `where` precondition — a cached [[UnifiedModuleNames]] membership test for its
    * [[whereCompanionName]] in the compiler pool (where [[MetaWhereDesugarer]] emits the `^Where` companion).
    */
  private def hasWhereCompanion(callee: ValueFQN): CompilerIO[Boolean] =
    getFactOrAbort(UnifiedModuleNames.Key(callee.moduleName, Platform.Compiler))
      .map(_.names.contains(whereCompanionName(callee)))

  private def rejectWhereAsValueIfBearing(node: Sourced[MonomorphicExpression], callee: ValueFQN): CompilerIO[Unit] =
    hasWhereCompanion(callee).ifM(rejectWhereAsValue(node, callee), ().pure[CompilerIO])

  /** The §2.1 fail-safe: a reference to a `where`-bearing def that is not the head of a full application — a bare value
    * reference or a partial application — is a use whose precondition can never be demanded. Reject it with a loud,
    * conservative error rather than silently accept the escape. (Lifting the restriction later would need value-level
    * tracking of the precondition through the function value, which the channel deliberately does not have.)
    */
  private def rejectWhereAsValue(node: Sourced[MonomorphicExpression], callee: ValueFQN): CompilerIO[Unit] =
    Sourced.compilerError(
      node.as(s"A def with a `where` precondition ('${callee.show}') cannot be passed as a value."),
      Seq("Call it directly with all its arguments so its precondition is checked at the use site.")
    )

  /** Evaluate a resolved `^Where` companion over the call's argument metas and turn the verdict into a use-site error or
    * a pass. Every argument's meta must be known (⊤ cannot discharge a demand — the fail-safe of §4.3); the predicate
    * then reduces (through the one NbE evaluator, over the arguments' meta values) to a `Bool`: `true` passes, `false` is
    * a violation, and a non-`Bool` result (an unsupported predicate shape) fails loudly rather than silently accepting.
    */
  private def demandPrecondition(
      callNode: Sourced[MonomorphicExpression],
      callee: ValueFQN,
      argMetas: Seq[Option[GroundValue]]
  ): CompilerIO[Unit] =
    argMetas.sequence match {
      case None        =>
        Sourced.compilerError(
          callNode.as(s"Cannot prove the precondition of '${callee.show}': an argument's value range is not known here."),
          Seq("A `where` precondition demands a provable range — pass a value whose range the compiler can determine.")
        )
      case Some(metas) =>
        // Reduce the `^Where` companion over the argument metas through the escalating linker-executor (same executor as
        // the transfer/merge path), then read the verdict: `true` passes, `false` is a violation, and anything else — a
        // non-`Bool` predicate shape, or a companion that did not reduce — fails loudly rather than silently accepting.
        EscalatingReducer.reduceApplied(whereCompanionFqn(callee), Seq.empty, metas.map(Evaluator.groundToSem)).flatMap {
          case Some(gv) if isBoolTrue(gv)  => ().pure[CompilerIO]
          case Some(gv) if isBoolFalse(gv) =>
            Sourced.compilerError(
              callNode.as(s"The precondition of '${callee.show}' is not satisfied by the argument's value range.")
            )
          case _                           =>
            Sourced.compilerError(callNode.as(s"Cannot evaluate the `where` precondition of '${callee.show}'."))
        }
    }

  private def isBoolTrue(gv: GroundValue): Boolean = gv match {
    case GroundValue.Direct(Literal.BooleanValue(true), _) => true
    case _                           => false
  }

  private def isBoolFalse(gv: GroundValue): Boolean = gv match {
    case GroundValue.Direct(Literal.BooleanValue(false), _) => true
    case _                            => false
  }

  /** Flatten a curried application into its ultimate head and its arguments in source order. */
  private def flatten(
      node: Sourced[MonomorphicExpression]
  ): (Sourced[MonomorphicExpression], Seq[Sourced[MonomorphicExpression]]) =
    node.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        val (head, args) = flatten(target)
        (head, args :+ argument)
      case _                                                           => (node, Seq.empty)
    }

}

object RefinementChannelProcessor {

  /** The `BigInteger` type an integer literal's value carries — the only literal-domain constant the channel needs, to
    * wrap a literal `n` as a [[GroundValue.Direct]] when reducing `integerLiteral^Meta` at it (the seed's *construction*
    * lives in Eliot; the channel only supplies the raw value). At the literal-protocol level, not the tracking domain —
    * and the canonical [[WellKnownTypes.bigIntFQN]], not a re-spelled FQN.
    */
  private val bigIntType: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  /** A callee's `^Meta` companion FQN: its own name in the [[Qualifier.Meta]] namespace, same module — what
    * [[MetaTransferDesugarer]] emits from a return brace. `fold` ⤳ `fold^Meta` (merge), the `Numeric[Int]` `add` ⤳
    * `add^Meta` (transfer). Keeping only `callee.name.name` also *strips* an ability-impl callee's `(ability, pattern)`
    * qualifier down to plain `Meta`, so the arithmetic instance method's companion resolves. The channel recognises a
    * refinement operation *only* by the presence of this companion — no leaf or branch construct is named.
    */
  private[channel] def metaCompanionFqn(callee: ValueFQN): ValueFQN =
    ValueFQN(callee.moduleName, QualifiedName(callee.name.name, Qualifier.Meta))

  private val unitModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "Unit")

  /** `Unit` — the meta type of every untracked (slotless) type. Its trivial `Meta[Unit]` instance (declared with `Unit`)
    * is the do-nothing join, so a merge over untracked arms reduces cleanly to no refinement (⊤).
    */
  private[channel] val unitType: GroundValue =
    GroundValue.Structure(ValueFQN(unitModule, QualifiedName("Unit", Qualifier.Type)), Seq.empty, GroundValue.Type)

  /** The name / FQN of a def's `^Where` companion (bounds-as-refinements §4.3): the def's own name suffixed with
    * [[MetaWhereDesugarer.whereSuffix]], in the [[Qualifier.Meta]] namespace and the def's own module — exactly what
    * [[MetaWhereDesugarer]] emits, so the channel finds the precondition companion `MetaWhereDesugarer` generated.
    */
  private[channel] def whereCompanionName(callee: ValueFQN): QualifiedName =
    QualifiedName(callee.name.name + MetaWhereDesugarer.whereSuffix, Qualifier.Meta)

  private[channel] def whereCompanionFqn(callee: ValueFQN): ValueFQN =
    ValueFQN(callee.moduleName, whereCompanionName(callee))
}
