package com.vanillasource.eliot.eliotc.saturate.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.core.fact.{RoleHint, TypeStack}
import com.vanillasource.eliot.eliotc.effect.fact.EffectDesugaredValue
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleName,
  QualifiedName,
  Qualifier,
  UnifiedModuleNames,
  ValueFQN,
  WellKnownTypes
}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  SignatureView,
  applyChain,
  arrow,
  isFunctionReference,
  spine
}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Saturates parameter-position bare references to omittable (`auto`-marked) type constructors (implicit-generics,
  * W1).
  *
  * For each value, the *signature* (type-stack level 0) is walked: every value-parameter type whose head is a bare
  * under-applied omittable constructor (e.g. `Int`, whose leading parameters `MIN`/`MAX` are `auto`) is rewritten into
  * an explicit application over fresh binders (`Int` → `Int[$Int$0, $Int$1]`), and those binders are prepended to the
  * value's generic prefix with the matching kind level synthesized/extended. Each bare occurrence mints *independent*
  * binders (matching the hand-written `+`, whose left/right ranges differ).
  *
  * The signature's curried-arrow structure is read and rebuilt through [[OperatorResolvedExpression.SignatureView]] (the
  * shared leading-binders / `Function`-domains / return-position view), so this processor only expresses *what* changes
  * per position, not how to walk the arrow chain.
  *
  * Scope of W1:
  *   - Only *parameter* positions generalize. The final return position is left untouched (it is *calculated* — W3).
  *   - Function-typed parameters (arrows) are not descended into (calculated indices through higher-order arguments are
  *     out of scope — Limit 6); bare references nested inside ordinary applied constructors (e.g. `List[Int]`) are
  *     saturated.
  *   - A bare reference to a non-omittable constructor (`inferableArity == 0`, e.g. `IO`) is left under-applied, so the
  *     ordinary type check still rejects it — the use-site guardrail, body-free.
  *
  * A value with no parameter-position bare omittable reference passes its [[OperatorResolvedValue]] through unchanged.
  */
class SaturatedValueProcessor
    extends TransformationProcessor[EffectDesugaredValue.Key, SaturatedValue.Key](key =>
      EffectDesugaredValue.Key(key.vfqn, key.platform)
    ) {

  override protected def generateFromKeyAndFact(
      key: SaturatedValue.Key,
      effectDesugared: EffectDesugaredValue
  ): CompilerIO[SaturatedValue] = {
    given Platform = effectDesugared.value.platform
    val value      = effectDesugared.value
    dataSaturate(value).flatMap {
      case Some(rewritten) => rewritten.pure[CompilerIO]
      case None            => saturate(value)
    }.map(SaturatedValue.apply)
  }

  /** Fresh generic binder synthesized for one omitted, omittable parameter: its (unique within the value) name and its
    * kind (the referenced parameter's type restriction, e.g. `BigInteger` for `Int`'s bounds).
    */
  private case class FreshBinder(name: String, kind: OperatorResolvedExpression)

  private def saturate(value: OperatorResolvedValue)(using Platform): CompilerIO[OperatorResolvedValue] = {
    val pos       = value.name
    val signature = value.typeStack.as(value.typeStack.value.signature)
    val view      = SignatureView.of(signature)
    for {
      (newParams, binders) <- saturateParams(view.parameters, pos)
      // The return position is left untouched by the parameter rewrite, so detect on the original signature: a bare
      // under-applied omittable return is *calculated* (W3), filled from the body rather than the source type stack.
      calculatedReturn     <- detectCalculatedReturn(signature)
      // A bare under-applied omittable return (e.g. `Int`, kind `BigInteger → … → Type`) is kind-ill-formed in the
      // codomain of `Function`, which the type-stack kind-check requires to be `Type`. Replace it with the kind-correct
      // `Type` placeholder; the monomorphize checker swaps that placeholder for a fresh metavariable the callee's body
      // solves, and the caller recognises it (a `VType` return on a calculated-return producer) to read the callee's
      // monomorphized return instead. The real (computed) return never lives in the source type stack — that is the
      // whole point of "calculated".
      rewritten             = view.withParameters(newParams)
      finalView             = if (calculatedReturn) rewritten.withReturnType(typeRef(pos)) else rewritten
      saturated             =
        if (binders.isEmpty && !calculatedReturn) value
        // Only the type stack changes. The runtime body is left exactly as-is — its value-parameter lambdas are
        // unannotated (a value's type stack is the single source of truth for parameter types; see
        // `CoreExpressionConverter.buildCurriedBody`), so the body-against-signature check takes each parameter's type
        // from the saturated `VPi` domain with nothing to keep in sync. No walking or counting of body structure.
        else prependBinders(value, finalView.toExpression, binders, pos)
    } yield saturated.copy(calculatedReturn = calculatedReturn)
  }

  /** Saturate each parameter-position type in turn, threading the running fresh-binder index and accumulating the fresh
    * binders minted across all parameters.
    */
  private def saturateParams(
      parameters: Seq[Sourced[OperatorResolvedExpression]],
      pos: Sourced[?]
  )(using Platform): CompilerIO[(Seq[Sourced[OperatorResolvedExpression]], Seq[FreshBinder])] =
    parameters
      .foldLeftM((Seq.empty[Sourced[OperatorResolvedExpression]], 0, Seq.empty[FreshBinder])) {
        case ((accParams, idx, accBinders), param) =>
          saturateType(param.value, idx, pos).map { case (newDom, idx2, binders) =>
            (accParams :+ param.as(newDom), idx2, accBinders ++ binders)
          }
      }
      .map { case (params, _, binders) => (params, binders) }

  /** Whether the value's return position is a bare under-applied omittable reference — a *calculated* return
    * (implicit-generics, W3). The return is the signature's [[SignatureView.returnType]]; it is calculated iff
    * that head is an omittable type constructor (W1/W2 arity, via [[inferableInfo]]) applied to fewer arguments than its
    * omittable arity (e.g. a bare `Int`, or a bare W2-grown `Counter`). An explicit `Int[0, 255]`, a fully-applied
    * `IO[Unit]`, a non-omittable head (`String`), or a type-parameter return (`R`, a
    * [[OperatorResolvedExpression.ParameterReference]]) is not.
    */
  private def detectCalculatedReturn(signature: Sourced[OperatorResolvedExpression])(using Platform): CompilerIO[Boolean] = {
    val (head, args) = spine(SignatureView.of(signature).returnType.value)
    head match {
      case _: OperatorResolvedExpression.ValueReference if !isFunctionReference(head) =>
        inferableInfo(headFqn(head)).map(_.exists { case (arity, _) => args.length < arity })
      case _                                                                          => false.pure[CompilerIO]
    }
  }

  /** Prepend `binders` as leading generic `FunctionLiteral`s to `signature` and synthesize/extend the value's kind
    * level to `binder.kind → … → existingKind`, returning the value with the rewritten two-level type stack.
    */
  private def prependBinders(
      value: OperatorResolvedValue,
      signature: OperatorResolvedExpression,
      binders: Seq[FreshBinder],
      pos: Sourced[?]
  ): OperatorResolvedValue = {
    val existingKind = value.typeStack.value.levels.tail.headOption
      .getOrElse(OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.typeFQN)))
    val newKind      = binders.foldRight(existingKind)((b, acc) => arrow(pos.as(b.kind), pos.as(acc)))
    val withBinders  = binders.foldRight(signature) { (b, acc) =>
      OperatorResolvedExpression.FunctionLiteral(pos.as(b.name), Some(pos.as(TypeStack.of(b.kind))), pos.as(acc))
    }
    value.copy(typeStack = value.typeStack.as(TypeStack(NonEmptySeq.of(withBinders, newKind))))
  }

  /** Saturate bare omittable references inside one parameter type. Recurses into applied constructors (`List[Int]`) but
    * not into function arrows (cost-in-arrow is out of scope). Under-applied omittable references get their omitted
    * leading parameters filled with fresh binders.
    */
  private def saturateType(
      expr: OperatorResolvedExpression,
      idx: Int,
      pos: Sourced[?]
  )(using Platform): CompilerIO[(OperatorResolvedExpression, Int, Seq[FreshBinder])] = {
    val (head, args) = spine(expr)
    head match {
      case ref: OperatorResolvedExpression.ValueReference if !isFunctionReference(head) =>
        for {
          infoOpt                              <- inferableInfo(ref.valueName.value)
          (saturatedArgsRev, idxA, argBinders) <- args.foldLeftM(
                                                     (List.empty[Sourced[OperatorResolvedExpression]], idx, Seq.empty[FreshBinder])
                                                   ) { case ((acc, i, bs), arg) =>
                                                     saturateType(arg.value, i, pos).map { case (na, i2, nb) =>
                                                       (arg.as(na) :: acc, i2, bs ++ nb)
                                                     }
                                                   }
          saturatedArgs                         = saturatedArgsRev.reverse
          result                                = infoOpt match {
                                                    case Some((arity, kinds)) if args.length < arity =>
                                                      val (freshRefs, freshBinders, idxF) =
                                                        (args.length until arity).foldLeft(
                                                          (Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[FreshBinder], idxA)
                                                        ) { case ((refs, binders, i), paramIdx) =>
                                                          val name = freshName(ref.valueName.value, i)
                                                          val kind = kinds.lift(paramIdx).getOrElse(typeRef(pos))
                                                          (
                                                            refs :+ pos.as(
                                                              OperatorResolvedExpression.ParameterReference(pos.as(name))
                                                            ),
                                                            binders :+ FreshBinder(name, kind),
                                                            i + 1
                                                          )
                                                        }
                                                      (applyChain(pos.as(head), saturatedArgs ++ freshRefs), idxF, argBinders ++ freshBinders)
                                                    case _                                            =>
                                                      (applyChain(pos.as(head), saturatedArgs), idxA, argBinders)
                                                  }
        } yield result
      case _                                                                           =>
        (expr, idx, Seq.empty[FreshBinder]).pure[CompilerIO]
    }
  }

  /** Look up an omittable reference's leading-`auto` arity and the kinds (type restrictions) of those leading
    * parameters, including the binders a W2-grown record data type carries (so a bare reference to `Counter` — whose
    * raw type-constructor arity is 0, the field bounds being added by W2 — is still recognised as omittable here, with
    * arity 2 and kinds `BigInteger, BigInteger`). [[None]] for a non-omittable or unresolvable reference (treated as
    * not saturable, so the ordinary check still rejects a bare under-applied use).
    *
    * This is the omittability oracle for *function-signature* positions — W1 parameter saturation ([[saturateType]])
    * and the W3 calculated-return detection ([[detectCalculatedReturn]]). Data-field contributions ([[fieldContribution]])
    * deliberately use the *raw* arity instead, so bounds do not yet propagate transitively through nested records
    * (the "viral bounds" of W5); that also keeps this growth lookup non-recursive (it reads only raw field arities).
    */
  private def inferableInfo(fqn: ValueFQN)(using platform: Platform): CompilerIO[Option[(Int, Seq[OperatorResolvedExpression])]] =
    getFact(OperatorResolvedValue.Key(fqn, platform)).flatMap {
      case Some(orv) =>
        recordGrowth(fqn).map { case (extraArity, extraKinds) =>
          val arity = orv.inferableArity + extraArity
          val kinds = leadingBinderKinds(signatureOf(orv)) ++ extraKinds
          Option.when(arity > 0)((arity, kinds))
        }
      case None      => none[(Int, Seq[OperatorResolvedExpression])].pure[CompilerIO]
    }

  /** The W2 growth of a *type-constructor* reference: the binders (and their kinds) a record data type gains from its
    * bare omittable fields, mirroring [[growTypeConstructor]]. `(0, empty)` for a non-type-constructor reference or a
    * non-record (abstract `type`, no value constructor). Reads only the value constructor's *raw* field arities (via
    * [[recordPlanFor]] ⟶ [[fieldContribution]]), never another value's growth, so it cannot recurse.
    */
  private def recordGrowth(fqn: ValueFQN)(using Platform): CompilerIO[(Int, Seq[OperatorResolvedExpression])] =
    if (fqn.name.qualifier == Qualifier.Type)
      recordPlanFor(fqn.moduleName, fqn.name.name).map {
        case Some(plan) => (plan.total, plan.allKinds)
        case None       => (0, Seq.empty)
      }
    else (0, Seq.empty[OperatorResolvedExpression]).pure[CompilerIO]

  /** The *raw* leading-`auto` arity and kinds of a reference — its own operator-resolved markers only, with no W2
    * record growth. Used by [[fieldContribution]] so a record field's bounds do not propagate transitively (W5).
    */
  private def rawInferableInfo(fqn: ValueFQN)(using platform: Platform): CompilerIO[Option[(Int, Seq[OperatorResolvedExpression])]] =
    getFact(OperatorResolvedValue.Key(fqn, platform)).map {
      case Some(orv) if orv.inferableArity > 0 => Some((orv.inferableArity, leadingBinderKinds(signatureOf(orv))))
      case _                                   => None
    }

  /** The kinds of a value's leading generic binders, in declaration order. Two signature shapes carry these:
    *
    *   - a value `def`-with-generics signature is a [[SignatureView.binders]] chain, where each binder's parameter-type
    *     annotation is its kind (`A: Type` → `Type`);
    *   - an abstract type-constructor (`type Int[MIN: BigInteger, ...]`) signature *is* its kind chain
    *     `Function[BigInteger, …, Type]`, where each [[SignatureView.parameters]] domain is the corresponding
    *     parameter's kind.
    *
    * Appending the binder kinds and the parameter-domain kinds covers both uniformly (one list is always empty), so the
    * saturated binders carry the correct kind (`BigInteger` for `Int`'s bounds), not a `Type` fallback.
    */
  private def leadingBinderKinds(sig: Sourced[OperatorResolvedExpression]): Seq[OperatorResolvedExpression] = {
    val view = SignatureView.of(sig)
    view.binders.map(b => b.parameterType.map(_.value.signature).getOrElse(typeRef(b.name))) ++
      view.parameters.map(_.value)
  }

  /** A value's signature (type-stack level 0) carried with its source position, for [[SignatureView.of]]. */
  private def signatureOf(value: OperatorResolvedValue): Sourced[OperatorResolvedExpression] =
    value.typeStack.as(value.typeStack.value.signature)

  private def typeRef(pos: Sourced[?]): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.typeFQN))

  /** A synthesized binder name, unique within the value (the running index disambiguates repeated references). The `$`
    * prefix cannot collide with user-written parameter names.
    */
  private def freshName(fqn: ValueFQN, idx: Int): String = s"$$${fqn.name.name}$$$idx"

  // ----- Implicit / inferred generics: data-field saturation (W2) -----
  //
  // A bare omittable field (`data Counter(n: Int)`) generalizes like a parameter (W1), but — unlike W1's independent
  // per-occurrence binders — the synthesized bounds must be *shared* across the data type's whole desugared family: the
  // type constructor grows to carry them (`Counter[lo, hi]`), and the value constructor's result, the accessors and the
  // auto-generated match implementations all reference those same binders, so construct / access / match round-trip with
  // the field's bounds. This is the cornerstone-sanctioned generic-data shape (`data Counter[lo, hi](n: Int[lo, hi])`)
  // auto-filled from the field's leading-`auto` arity — which is only knowable post-resolution, hence here and not in
  // the (pre-resolution) `DataDefinitionDesugarer`.
  //
  // Scope: single-constructor records (the constructor's local name equals the data type's) with directly-bare omittable
  // fields. Other shapes fall through to the ordinary W1 saturation.

  /** One data field's contribution to its type's synthesized binder list: how many leading omittable bounds it omits,
    * their kinds (`Int` ⇒ count 2, kinds `BigInteger, BigInteger`; an explicit `Int[0, 255]` ⇒ count 0) and — for a
    * contributing field — its (omittable) type-constructor FQN, used to recognise the field type inside the auto-generated
    * match implementations.
    */
  private case class FieldContribution(count: Int, kinds: Seq[OperatorResolvedExpression], fqn: Option[ValueFQN])

  /** The shared binder plan for one record data type, derived from its constructor's fields. The binders are named
    * `$<Type>$<i>`; each function of the data family re-derives the same plan and names them identically, but they are
    * only ever its own local generic parameters (names need not, and do not, cross function boundaries).
    */
  private case class TypePlan(typeName: String, fields: Seq[FieldContribution]) {
    val total: Int                                = fields.map(_.count).sum
    def allKinds: Seq[OperatorResolvedExpression] = fields.flatMap(_.kinds)
    def offset(fieldIndex: Int): Int              = fields.take(fieldIndex).map(_.count).sum
    def autoFieldIndices: Seq[Int]                = fields.zipWithIndex.collect { case (f, i) if f.count > 0 => i }
    private def binderName(i: Int): String        = s"$$$typeName$$$i"
    def binders: Seq[FreshBinder]                 = (0 until total).map(i => FreshBinder(binderName(i), allKinds(i)))
    private def ref(i: Int, pos: Sourced[?]): Sourced[OperatorResolvedExpression] =
      pos.as(OperatorResolvedExpression.ParameterReference(pos.as(binderName(i))))
    def allRefs(pos: Sourced[?]): Seq[Sourced[OperatorResolvedExpression]]        = (0 until total).map(ref(_, pos))
    def sliceRefs(fieldIndex: Int, pos: Sourced[?]): Seq[Sourced[OperatorResolvedExpression]] =
      (offset(fieldIndex) until offset(fieldIndex) + fields(fieldIndex).count).map(ref(_, pos))
  }

  private sealed trait DataRole
  private object DataRole {
    case class TypeCtor(plan: TypePlan)             extends DataRole
    case class ValueCtor(plan: TypePlan)            extends DataRole
    case class Accessor(plan: TypePlan, field: Int) extends DataRole
    case class ImplMember(plan: TypePlan)           extends DataRole
  }

  private def dataSaturate(value: OperatorResolvedValue)(using Platform): CompilerIO[Option[OperatorResolvedValue]] =
    classifyDataRole(value).map(_.map {
      case DataRole.TypeCtor(plan)        => growTypeConstructor(value, plan)
      case DataRole.ValueCtor(plan)       => saturateValueConstructor(value, plan)
      case DataRole.Accessor(plan, field) => saturateAccessor(value, plan, field)
      case DataRole.ImplMember(plan)      => saturateImplMember(value, plan)
    })

  /** Decide whether `value` is part of a saturable record data family, and with what plan / position. The role is read
    * from the (forwarded) [[RoleHint]] for constructors/accessors, from the `PatternMatch`/`TypeMatch` qualifier for the
    * auto-generated match implementations, and from the `Type` qualifier for type constructors — never from
    * `RoleHint.TypeConstructor` (its count is cornerstone write-only); identifying a type constructor by its qualifier
    * and looking its plan up from the value constructor stays out of the kind/arity-metadata channel.
    */
  private def classifyDataRole(value: OperatorResolvedValue)(using Platform): CompilerIO[Option[DataRole]] =
    value.roleHint match {
      case RoleHint.ValueConstructor(dataType, _) if dataType.name == value.vfqn.name.name =>
        recordPlanFor(value.vfqn.moduleName, dataType.name).map(_.map(DataRole.ValueCtor.apply))
      case RoleHint.FieldAccessor(dataType, fieldIndex)                                    =>
        recordPlanFor(value.vfqn.moduleName, dataType.name).map(_.map(DataRole.Accessor(_, fieldIndex)))
      case _ if isMatchImplMember(value.vfqn)                                              =>
        matchImplDataType(value.vfqn).flatMap {
          case Some(typeName) => recordPlanFor(value.vfqn.moduleName, typeName).map(_.map(DataRole.ImplMember.apply))
          case None           => none[DataRole].pure[CompilerIO]
        }
      case _ if value.vfqn.name.qualifier == Qualifier.Type                                =>
        recordPlanFor(value.vfqn.moduleName, value.vfqn.name.name).map(_.map(DataRole.TypeCtor.apply))
      case _                                                                               =>
        none[DataRole].pure[CompilerIO]
    }

  private def isMatchImplMember(vfqn: ValueFQN): Boolean =
    WellKnownTypes.isPatternMatchImplementation(vfqn) || WellKnownTypes.isTypeMatchImplementation(vfqn)

  /** The data type a `PatternMatch`/`TypeMatch` implementation member is for, recovered from its impl marker (the
    * marker's first argument references the data type constructor by name).
    */
  private def matchImplDataType(vfqn: ValueFQN)(using platform: Platform): CompilerIO[Option[String]] = {
    val abilityName =
      if (WellKnownTypes.isPatternMatchImplementation(vfqn)) WellKnownTypes.patternMatchAbilityName
      else WellKnownTypes.typeMatchAbilityName
    ImplementationMarkerUtils.firstPatternTypeConstructorName(vfqn, abilityName, platform)
  }

  /** The binder plan for the record data type `typeName` in `module`, read off its value constructor's fields. `None`
    * unless `typeName` names a single-constructor record (its constructor's local name equals `typeName`) with at least
    * one directly-bare omittable field.
    *
    * The value-constructor name's *existence* is checked against [[UnifiedModuleNames]] first: requesting a missing
    * value's [[OperatorResolvedValue]] aborts the chain with a "Could not find" error (see `UnifiedModuleValueProcessor`),
    * which would otherwise be spuriously triggered for every abstract `Type` (`Int`, `Bool`, …) that has no constructor.
    */
  private def recordPlanFor(module: ModuleName, typeName: String)(using platform: Platform): CompilerIO[Option[TypePlan]] = {
    val ctorName = QualifiedName(typeName, Qualifier.Default)
    getFact(UnifiedModuleNames.Key(module, platform)).flatMap {
      case Some(names) if names.names.contains(ctorName) =>
        getFact(OperatorResolvedValue.Key(ValueFQN(module, ctorName), platform)).flatMap {
          case Some(ctor) if isRecordConstructor(ctor, typeName) =>
            SignatureView.of(signatureOf(ctor)).parameters.map(_.value).traverse(fieldContribution).map { fields =>
              val plan = TypePlan(typeName, fields)
              Option.when(plan.total > 0)(plan)
            }
          case _                                                 => none[TypePlan].pure[CompilerIO]
        }
      case _                                             => none[TypePlan].pure[CompilerIO]
    }
  }

  private def isRecordConstructor(ctor: OperatorResolvedValue, typeName: String): Boolean =
    ctor.roleHint match {
      case RoleHint.ValueConstructor(dataType, _) => dataType.name == typeName && ctor.vfqn.name.name == typeName
      case _                                      => false
    }

  /** The omittable-bound contribution of one field type: its head's omitted leading `auto` count and their kinds. A
    * directly-bare omittable head (`Int`) contributes; an explicit or non-omittable head contributes nothing.
    */
  private def fieldContribution(fieldType: OperatorResolvedExpression)(using Platform): CompilerIO[FieldContribution] = {
    val (head, args) = spine(fieldType)
    head match {
      case _: OperatorResolvedExpression.ValueReference if !isFunctionReference(head) =>
        rawInferableInfo(headFqn(head)).map {
          case Some((arity, kinds)) if args.length < arity =>
            FieldContribution(arity - args.length, kinds.slice(args.length, arity), Some(headFqn(head)))
          case _                                           => FieldContribution(0, Seq.empty, None)
        }
      case _                                                                          => FieldContribution(0, Seq.empty, None).pure[CompilerIO]
    }
  }

  private def headFqn(expr: OperatorResolvedExpression): ValueFQN = expr match {
    case OperatorResolvedExpression.ValueReference(name, _) => name.value
    case _                                                  => WellKnownTypes.typeFQN
  }

  /** Grow the record type constructor's arity by the plan's binders: its single-level kind chain `… → Type` gains
    * `kind → … → Type` slots (a type constructor's params are value arguments, see `DataDefinitionDesugarer`), and its
    * `inferableArity` is bumped so bare references to it elsewhere saturate via the ordinary W1 path.
    */
  private def growTypeConstructor(value: OperatorResolvedValue, plan: TypePlan): OperatorResolvedValue = {
    val pos          = value.name
    val view         = SignatureView.of(signatureOf(value))
    val newSignature = view.withParameters(view.parameters ++ plan.allKinds.map(pos.as)).toExpression
    value.copy(
      typeStack = value.typeStack.as(TypeStack.of(newSignature)),
      inferableArity = value.inferableArity + plan.total
    )
  }

  /** Saturate a record value constructor: each directly-bare omittable field gains its slice of the shared binders, the
    * result type is applied to *all* the binders (`Counter` ⇒ `Counter[lo, hi]`), and the binders are prepended as the
    * constructor's generic prefix. Construct sites infer the binders from the field arguments (ordinary "too few type
    * args" inference), exactly as for W1's synthesized parameter binders.
    */
  private def saturateValueConstructor(value: OperatorResolvedValue, plan: TypePlan): OperatorResolvedValue = {
    val pos       = value.name
    val view      = SignatureView.of(signatureOf(value))
    val newParams = view.parameters.zipWithIndex.map { case (dom, i) =>
      dom.as(applyChain(dom, plan.sliceRefs(i, pos)))
    }
    val newReturn = applyChain(view.returnType, plan.allRefs(pos))
    prependBinders(value, view.withParameters(newParams).withReturnType(newReturn).toExpression, plan.binders, pos)
  }

  /** Saturate a single field accessor: its `obj` parameter takes the data type applied to *all* binders and its result
    * is the projected field's type applied to *that field's* binder slice (`n(obj: Counter): Int` ⇒
    * `n(obj: Counter[lo, hi]): Int[lo, hi]`).
    */
  private def saturateAccessor(value: OperatorResolvedValue, plan: TypePlan, fieldIndex: Int): OperatorResolvedValue = {
    val pos       = value.name
    val view      = SignatureView.of(signatureOf(value))
    val newParams = view.parameters.map(dom => dom.as(applyChain(dom, plan.allRefs(pos))))
    val newReturn = applyChain(view.returnType, plan.sliceRefs(fieldIndex, pos))
    prependBinders(value, view.withParameters(newParams).withReturnType(newReturn).toExpression, plan.binders, pos)
  }

  /** Saturate one auto-generated `PatternMatch`/`TypeMatch` implementation member (marker, `Cases`/`Fields`, the
    * `handleCases` eliminator, the `typeMatch` matcher): every reference to the grown data type becomes
    * `T[lo, hi, …]` and every embedded omittable field type (in the `handleCases` Church selector) is applied to that
    * field's binder slice, so the implementation stays consistent with the grown type constructor and surface `match`
    * round-trips with the field's bounds. The shared binders are prepended as the implementation's leading generic
    * parameters, in front of any member-specific binder (e.g. the result `R`).
    */
  private def saturateImplMember(value: OperatorResolvedValue, plan: TypePlan): OperatorResolvedValue = {
    val pos          = value.name
    val dataTypeFqn  = ValueFQN(value.vfqn.moduleName, QualifiedName(plan.typeName, Qualifier.Type))
    val applied      = applyPlanUnderBinders(value.typeStack.value.signature, plan, dataTypeFqn, pos)
    val newSignature =
      if (WellKnownTypes.isTypeMatchTypeMatch(value.vfqn)) rebuildTypeMatchHandler(applied, plan, pos) else applied
    prependBinders(value, newSignature, plan.binders, pos)
  }

  /** Rebuild the `typeMatch` matcher's `matchCase` handler so a type-level match `case T[g.., lo, hi] -> ...` can bind
    * the W2-synthesized bounds. The desugarer built that handler from only the desugar-time generic params (a record
    * with none yields `Function[Unit, R]`); W2 grows the type constructor by the field bounds, so the handler must take
    * one curried argument per *grown* generic param — the explicit generics' kinds (recovered from the matcher's own
    * leading binders) followed by the W2 binder kinds — ending in the result `R`. The native applies the handler across
    * the whole spine of the matched type value, so its arity must equal the grown type constructor's. The handler is the
    * `matchCase` parameter, i.e. the second curried domain (after `obj: Type`).
    *
    * Only this matcher needs it: the abstract `Fields` associated type is bypassed — the match desugarer resolves a
    * surface type match straight to this concrete matcher, never through `Fields[R]`.
    */
  private def rebuildTypeMatchHandler(
      sig: OperatorResolvedExpression,
      plan: TypePlan,
      pos: Sourced[?]
  ): OperatorResolvedExpression = {
    val view          = SignatureView.of(pos.as(sig))
    val explicitKinds = view.binders.dropRight(1).map(b => b.parameterType.map(_.value.signature).getOrElse(typeRef(b.name)))
    val resultRef     = OperatorResolvedExpression.ParameterReference(view.binders.last.name)
    val newHandler    = (explicitKinds ++ plan.allKinds)
      .foldRight(resultRef: OperatorResolvedExpression)((k, acc) => arrow(pos.as(k), pos.as(acc)))
    val newParameters =
      if (view.parameters.sizeIs > 1) view.parameters.updated(1, view.parameters(1).as(newHandler))
      else view.parameters
    view.withParameters(newParameters).toExpression
  }

  /** Apply the data plan beneath any leading generic binders of an implementation member's signature, leaving the
    * binders themselves untouched (the shared binders are prepended separately).
    */
  private def applyPlanUnderBinders(
      expr: OperatorResolvedExpression,
      plan: TypePlan,
      dataTypeFqn: ValueFQN,
      pos: Sourced[?]
  ): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(name, paramType, body) =>
        OperatorResolvedExpression
          .FunctionLiteral(name, paramType, body.as(applyPlanUnderBinders(body.value, plan, dataTypeFqn, pos)))
      case other                                                            =>
        applyPlan(other, plan, dataTypeFqn, pos, 0)._1
    }

  /** Walk a type expression, applying the data type's shared binders to every bare reference to the data type
    * (`Counter` ⇒ `Counter[lo, hi]`) and each successive bare omittable field-type reference — in declaration order —
    * to its own binder slice (`Int` ⇒ `Int[lo, hi]`). Returns the rewritten expression and the running count of
    * field-type references already consumed.
    */
  private def applyPlan(
      expr: OperatorResolvedExpression,
      plan: TypePlan,
      dataTypeFqn: ValueFQN,
      pos: Sourced[?],
      consumed: Int
  ): (OperatorResolvedExpression, Int) = {
    val (head, args)      = spine(expr)
    val (newArgsRev, idx) = args.foldLeft((List.empty[Sourced[OperatorResolvedExpression]], consumed)) {
      case ((acc, i), arg) =>
        val (rewritten, i2) = applyPlan(arg.value, plan, dataTypeFqn, pos, i)
        (arg.as(rewritten) :: acc, i2)
    }
    val newArgs           = newArgsRev.reverse
    val autoFields        = plan.autoFieldIndices
    head match {
      case OperatorResolvedExpression.ValueReference(name, _) if name.value == dataTypeFqn =>
        (applyChain(pos.as(head), newArgs ++ plan.allRefs(pos)), idx)
      case OperatorResolvedExpression.ValueReference(name, _)
          if idx < autoFields.length && plan.fields(autoFields(idx)).fqn.contains(name.value) =>
        val fieldIndex = autoFields(idx)
        (applyChain(pos.as(head), newArgs ++ plan.sliceRefs(fieldIndex, pos)), idx + 1)
      case _                                                                               =>
        (applyChain(pos.as(head), newArgs), idx)
    }
  }
}
