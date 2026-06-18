package com.vanillasource.eliot.eliotc.saturate.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.core.fact.{RoleHint, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleName,
  QualifiedName,
  Qualifier,
  UnifiedModuleNames,
  ValueFQN,
  WellKnownTypes
}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Saturates parameter-position bare references to omittable (`auto`-marked) type constructors (W1 of
  * `docs/implicit-generics-plan.md`).
  *
  * For each value, the *signature* (type-stack level 0) is walked: every value-parameter type whose head is a bare
  * under-applied omittable constructor (e.g. `Int`, whose leading parameters `MIN`/`MAX` are `auto`) is rewritten into
  * an explicit application over fresh binders (`Int` → `Int[$Int$0, $Int$1]`), and those binders are prepended to the
  * value's generic prefix with the matching kind level synthesized/extended. Each bare occurrence mints *independent*
  * binders (matching the hand-written `+`, whose left/right ranges differ).
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
    extends TransformationProcessor[OperatorResolvedValue.Key, SaturatedValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  override protected def generateFromKeyAndFact(
      key: SaturatedValue.Key,
      value: OperatorResolvedValue
  ): CompilerIO[SaturatedValue] =
    dataSaturate(value).flatMap {
      case Some(rewritten) => rewritten.pure[CompilerIO]
      case None            => saturate(value)
    }.map(SaturatedValue.apply)

  /** Fresh generic binder synthesized for one omitted, omittable parameter: its (unique within the value) name and its
    * kind (the referenced parameter's type restriction, e.g. `BigInteger` for `Int`'s bounds).
    */
  private case class FreshBinder(name: String, kind: OperatorResolvedExpression)

  private def saturate(value: OperatorResolvedValue): CompilerIO[OperatorResolvedValue] = {
    val pos = value.name
    rewriteSignature(value.typeStack.value.signature, 0, pos).map { case (newSignature, _, binders) =>
      if (binders.isEmpty) value
      // Only the type stack changes. The runtime body is left exactly as-is — its value-parameter lambdas are
      // unannotated (a value's type stack is the single source of truth for parameter types; see
      // `CoreExpressionConverter.buildCurriedBody`), so the body-against-signature check takes each parameter's type
      // from the saturated `VPi` domain with nothing to keep in sync. No walking or counting of body structure.
      else prependBinders(value, newSignature, binders, pos)
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
    val newKind      = binders.foldRight(existingKind)((b, acc) => functionType(b.kind, acc, pos))
    val withBinders  = binders.foldRight(signature) { (b, acc) =>
      OperatorResolvedExpression.FunctionLiteral(pos.as(b.name), Some(pos.as(TypeStack.of(b.kind))), pos.as(acc))
    }
    value.copy(typeStack = value.typeStack.as(TypeStack(NonEmptySeq.of(withBinders, newKind))))
  }

  /** Walk the signature: preserve existing leading generic binders ([[OperatorResolvedExpression.FunctionLiteral]]s),
    * then rewrite the function-type chain underneath them. Returns the rewritten signature body (without the new
    * binders — they are prepended by [[saturate]]), the running fresh-binder index, and the collected fresh binders.
    */
  private def rewriteSignature(
      expr: OperatorResolvedExpression,
      idx: Int,
      pos: Sourced[?]
  ): CompilerIO[(OperatorResolvedExpression, Int, Seq[FreshBinder])] =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramType, body) =>
        rewriteSignature(body.value, idx, pos).map { case (newBody, idx2, binders) =>
          (OperatorResolvedExpression.FunctionLiteral(paramName, paramType, body.as(newBody)), idx2, binders)
        }
      case other                                                                  =>
        rewriteFunctionChain(other, idx, pos)
    }

  /** Walk the curried `Function[dom, cod]` chain of a signature, saturating each *parameter* type (`dom`) and recursing
    * into the codomain. The final non-`Function` head is the return position and is left untouched (W3).
    */
  private def rewriteFunctionChain(
      expr: OperatorResolvedExpression,
      idx: Int,
      pos: Sourced[?]
  ): CompilerIO[(OperatorResolvedExpression, Int, Seq[FreshBinder])] =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, cod) =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, dom) if isFunctionRef(fn.value) =>
            for {
              (newDom, idx1, domBinders) <- saturateType(dom.value, idx, pos)
              (newCod, idx2, codBinders) <- rewriteFunctionChain(cod.value, idx1, pos)
            } yield (
              OperatorResolvedExpression
                .FunctionApplication(target.as(OperatorResolvedExpression.FunctionApplication(fn, dom.as(newDom))), cod.as(newCod)),
              idx2,
              domBinders ++ codBinders
            )
          case _                                                                                  =>
            (expr, idx, Seq.empty[FreshBinder]).pure[CompilerIO]
        }
      case _                                                           =>
        (expr, idx, Seq.empty[FreshBinder]).pure[CompilerIO]
    }

  /** Saturate bare omittable references inside one parameter type. Recurses into applied constructors (`List[Int]`) but
    * not into function arrows (cost-in-arrow is out of scope). Under-applied omittable references get their omitted
    * leading parameters filled with fresh binders.
    */
  private def saturateType(
      expr: OperatorResolvedExpression,
      idx: Int,
      pos: Sourced[?]
  ): CompilerIO[(OperatorResolvedExpression, Int, Seq[FreshBinder])] = {
    val (head, args) = spineOf(expr)
    head match {
      case ref: OperatorResolvedExpression.ValueReference if !isFunctionRef(head) =>
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
                                                      (applyAll(head, saturatedArgs ++ freshRefs, pos), idxF, argBinders ++ freshBinders)
                                                    case _                                            =>
                                                      (applyAll(head, saturatedArgs, pos), idxA, argBinders)
                                                  }
        } yield result
      case _                                                                     =>
        (expr, idx, Seq.empty[FreshBinder]).pure[CompilerIO]
    }
  }

  /** Look up an omittable reference's leading-`auto` arity and the kinds (type restrictions) of those leading
    * parameters, read off the referenced value's own operator-resolved signature. [[None]] for a non-omittable or
    * unresolvable reference (treated as not saturable, so the ordinary check still rejects a bare under-applied use).
    */
  private def inferableInfo(fqn: ValueFQN): CompilerIO[Option[(Int, Seq[OperatorResolvedExpression])]] =
    getFact(OperatorResolvedValue.Key(fqn)).map {
      case Some(orv) if orv.inferableArity > 0 =>
        Some((orv.inferableArity, leadingBinderKinds(orv.typeStack.value.signature)))
      case _                                   => None
    }

  /** The kinds of a referenced value's leading generic binders, in declaration order. Two signature shapes carry these:
    *
    *   - a value `def`-with-generics signature is a [[OperatorResolvedExpression.FunctionLiteral]] chain, where each
    *     binder's parameter-type annotation is its kind (`A: Type` → `Type`);
    *   - an abstract type-constructor (`type Int[MIN: BigInteger, ...]`) signature *is* its kind-chain
    *     `Function[BigInteger, Function[..., Type]]`, where each curried *domain* is the corresponding parameter's kind.
    *
    * Handling both lets the saturated binders carry the correct kind (`BigInteger` for `Int`'s bounds), not a `Type`
    * fallback.
    */
  private def leadingBinderKinds(sig: OperatorResolvedExpression): Seq[OperatorResolvedExpression] =
    sig match {
      case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
        paramType
          .map(_.value.signature)
          .getOrElse(OperatorResolvedExpression.ValueReference(body.as(WellKnownTypes.typeFQN))) +:
          leadingBinderKinds(body.value)
      case OperatorResolvedExpression.FunctionApplication(target, cod)    =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, dom) if isFunctionRef(fn.value) =>
            dom.value +: leadingBinderKinds(cod.value)
          case _                                                                                  => Seq.empty
        }
      case _                                                              => Seq.empty
    }

  /** Decompose a curried application into its head and argument list (left to right). */
  private def spineOf(
      expr: OperatorResolvedExpression
  ): (OperatorResolvedExpression, Seq[Sourced[OperatorResolvedExpression]]) =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, arg) =>
        val (head, args) = spineOf(target.value)
        (head, args :+ arg)
      case other                                                       => (other, Seq.empty)
    }

  /** Re-apply a head to a list of arguments via left-associated [[OperatorResolvedExpression.FunctionApplication]]s. */
  private def applyAll(
      head: OperatorResolvedExpression,
      args: Seq[Sourced[OperatorResolvedExpression]],
      pos: Sourced[?]
  ): OperatorResolvedExpression =
    args.foldLeft(head)((acc, arg) => OperatorResolvedExpression.FunctionApplication(pos.as(acc), arg))

  /** `Function[dom, cod]` as an operator-resolved type expression. */
  private def functionType(
      dom: OperatorResolvedExpression,
      cod: OperatorResolvedExpression,
      pos: Sourced[?]
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionApplication(
      pos.as(
        OperatorResolvedExpression.FunctionApplication(
          pos.as(OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.functionDataTypeFQN))),
          pos.as(dom)
        )
      ),
      pos.as(cod)
    )

  private def typeRef(pos: Sourced[?]): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.typeFQN))

  private def isFunctionRef(expr: OperatorResolvedExpression): Boolean = expr match {
    case OperatorResolvedExpression.ValueReference(name, _) => name.value == WellKnownTypes.functionDataTypeFQN
    case _                                                  => false
  }

  /** A synthesized binder name, unique within the value (the running index disambiguates repeated references). The `$`
    * prefix cannot collide with user-written parameter names.
    */
  private def freshName(fqn: ValueFQN, idx: Int): String = s"$$${fqn.name.name}$$$idx"

  // ----- Implicit / inferred generics: data-field saturation (W2 of docs/implicit-generics-plan.md) -----
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

  private def dataSaturate(value: OperatorResolvedValue): CompilerIO[Option[OperatorResolvedValue]] =
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
  private def classifyDataRole(value: OperatorResolvedValue): CompilerIO[Option[DataRole]] =
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
  private def matchImplDataType(vfqn: ValueFQN): CompilerIO[Option[String]] = {
    val abilityName =
      if (WellKnownTypes.isPatternMatchImplementation(vfqn)) WellKnownTypes.patternMatchAbilityName
      else WellKnownTypes.typeMatchAbilityName
    ImplementationMarkerUtils.firstPatternTypeConstructorName(vfqn, abilityName)
  }

  /** The binder plan for the record data type `typeName` in `module`, read off its value constructor's fields. `None`
    * unless `typeName` names a single-constructor record (its constructor's local name equals `typeName`) with at least
    * one directly-bare omittable field.
    *
    * The value-constructor name's *existence* is checked against [[UnifiedModuleNames]] first: requesting a missing
    * value's [[OperatorResolvedValue]] aborts the chain with a "Could not find" error (see `UnifiedModuleValueProcessor`),
    * which would otherwise be spuriously triggered for every abstract `Type` (`Int`, `Bool`, …) that has no constructor.
    */
  private def recordPlanFor(module: ModuleName, typeName: String): CompilerIO[Option[TypePlan]] = {
    val ctorName = QualifiedName(typeName, Qualifier.Default)
    getFact(UnifiedModuleNames.Key(module)).flatMap {
      case Some(names) if names.names.contains(ctorName) =>
        getFact(OperatorResolvedValue.Key(ValueFQN(module, ctorName))).flatMap {
          case Some(ctor) if isRecordConstructor(ctor, typeName) =>
            fieldTypes(ctor.typeStack.value.signature).traverse(fieldContribution).map { fields =>
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

  /** The curried `Function` domains of a value constructor's signature (its fields), skipping the leading generic
    * binders. The final non-`Function` head is the result type and is excluded.
    */
  private def fieldTypes(expr: OperatorResolvedExpression): Seq[OperatorResolvedExpression] =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(_, _, body)      => fieldTypes(body.value)
      case OperatorResolvedExpression.FunctionApplication(target, cod) =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, dom) if isFunctionRef(fn.value) =>
            dom.value +: fieldTypes(cod.value)
          case _                                                                                  => Seq.empty
        }
      case _                                                          => Seq.empty
    }

  /** The omittable-bound contribution of one field type: its head's omitted leading `auto` count and their kinds. A
    * directly-bare omittable head (`Int`) contributes; an explicit or non-omittable head contributes nothing.
    */
  private def fieldContribution(fieldType: OperatorResolvedExpression): CompilerIO[FieldContribution] = {
    val (head, args) = spineOf(fieldType)
    head match {
      case _: OperatorResolvedExpression.ValueReference if !isFunctionRef(head) =>
        inferableInfo(headFqn(head)).map {
          case Some((arity, kinds)) if args.length < arity =>
            FieldContribution(arity - args.length, kinds.slice(args.length, arity), Some(headFqn(head)))
          case _                                           => FieldContribution(0, Seq.empty, None)
        }
      case _                                                                    => FieldContribution(0, Seq.empty, None).pure[CompilerIO]
    }
  }

  private def headFqn(expr: OperatorResolvedExpression): ValueFQN = expr match {
    case OperatorResolvedExpression.ValueReference(name, _) => name.value
    case _                                                  => WellKnownTypes.typeFQN
  }

  /** Grow the record type constructor's arity by the plan's binders: its single-level signature `… → Type` gains
    * `kind → … → Type` slots (a type constructor's params are value arguments, see `DataDefinitionDesugarer`), and its
    * `inferableArity` is bumped so bare references to it elsewhere saturate via the ordinary W1 path.
    */
  private def growTypeConstructor(value: OperatorResolvedValue, plan: TypePlan): OperatorResolvedValue = {
    val pos          = value.name
    val newSignature = appendKinds(value.typeStack.value.signature, plan.allKinds, pos)
    value.copy(
      typeStack = value.typeStack.as(TypeStack.of(newSignature)),
      inferableArity = value.inferableArity + plan.total
    )
  }

  /** Insert `kinds` as additional curried domains just before the trailing `Type` of a type constructor's kind chain. */
  private def appendKinds(
      expr: OperatorResolvedExpression,
      kinds: Seq[OperatorResolvedExpression],
      pos: Sourced[?]
  ): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, cod) if isFunctionChain(target.value) =>
        OperatorResolvedExpression.FunctionApplication(target, cod.as(appendKinds(cod.value, kinds, pos)))
      case result                                                                                       =>
        kinds.foldRight(result)((k, acc) => functionType(k, acc, pos))
    }

  private def isFunctionChain(expr: OperatorResolvedExpression): Boolean = expr match {
    case OperatorResolvedExpression.FunctionApplication(fn, _) => isFunctionRef(fn.value)
    case _                                                     => false
  }

  /** Saturate a record value constructor: each directly-bare omittable field gains its slice of the shared binders, the
    * result type is applied to *all* the binders (`Counter` ⇒ `Counter[lo, hi]`), and the binders are prepended as the
    * constructor's generic prefix. Construct sites infer the binders from the field arguments (ordinary "too few type
    * args" inference), exactly as for W1's synthesized parameter binders.
    */
  private def saturateValueConstructor(value: OperatorResolvedValue, plan: TypePlan): OperatorResolvedValue = {
    val pos          = value.name
    val newSignature = rewriteConstructorBody(value.typeStack.value.signature, 0, plan, pos)
    prependBinders(value, newSignature, plan.binders, pos)
  }

  private def rewriteConstructorBody(
      expr: OperatorResolvedExpression,
      fieldIndex: Int,
      plan: TypePlan,
      pos: Sourced[?]
  ): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(name, paramType, body) =>
        OperatorResolvedExpression
          .FunctionLiteral(name, paramType, body.as(rewriteConstructorBody(body.value, fieldIndex, plan, pos)))
      case OperatorResolvedExpression.FunctionApplication(target, cod)       =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, dom) if isFunctionRef(fn.value) =>
            val newDom = applyAll(dom.value, plan.sliceRefs(fieldIndex, pos), pos)
            OperatorResolvedExpression.FunctionApplication(
              target.as(OperatorResolvedExpression.FunctionApplication(fn, dom.as(newDom))),
              cod.as(rewriteConstructorBody(cod.value, fieldIndex + 1, plan, pos))
            )
          case _                                                                                  =>
            applyAll(expr, plan.allRefs(pos), pos) // the result type: apply the data type to all shared binders
        }
      case result                                                           =>
        applyAll(result, plan.allRefs(pos), pos)
    }

  /** Saturate a single field accessor: its `obj` parameter takes the data type applied to *all* binders and its result
    * is the projected field's type applied to *that field's* binder slice (`n(obj: Counter): Int` ⇒
    * `n(obj: Counter[lo, hi]): Int[lo, hi]`).
    */
  private def saturateAccessor(value: OperatorResolvedValue, plan: TypePlan, fieldIndex: Int): OperatorResolvedValue = {
    val pos          = value.name
    val newSignature = value.typeStack.value.signature match {
      case OperatorResolvedExpression.FunctionApplication(target, cod) =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, dom) if isFunctionRef(fn.value) =>
            val newDom = applyAll(dom.value, plan.allRefs(pos), pos)
            val newCod = applyAll(cod.value, plan.sliceRefs(fieldIndex, pos), pos)
            OperatorResolvedExpression.FunctionApplication(
              target.as(OperatorResolvedExpression.FunctionApplication(fn, dom.as(newDom))),
              cod.as(newCod)
            )
          case _                                                                                  => value.typeStack.value.signature
        }
      case other                                                      => other
    }
    prependBinders(value, newSignature, plan.binders, pos)
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
    val newSignature = applyPlanUnderBinders(value.typeStack.value.signature, plan, dataTypeFqn, pos)
    prependBinders(value, newSignature, plan.binders, pos)
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
    val (head, args)         = spineOf(expr)
    val (newArgsRev, idx)    = args.foldLeft((List.empty[Sourced[OperatorResolvedExpression]], consumed)) {
      case ((acc, i), arg) =>
        val (rewritten, i2) = applyPlan(arg.value, plan, dataTypeFqn, pos, i)
        (arg.as(rewritten) :: acc, i2)
    }
    val newArgs              = newArgsRev.reverse
    val autoFields           = plan.autoFieldIndices
    head match {
      case OperatorResolvedExpression.ValueReference(name, _) if name.value == dataTypeFqn =>
        (applyAll(head, newArgs ++ plan.allRefs(pos), pos), idx)
      case OperatorResolvedExpression.ValueReference(name, _)
          if idx < autoFields.length && plan.fields(autoFields(idx)).fqn.contains(name.value) =>
        val fieldIndex = autoFields(idx)
        (applyAll(head, newArgs ++ plan.sliceRefs(fieldIndex, pos), pos), idx + 1)
      case _                                                                               =>
        (applyAll(head, newArgs, pos), idx)
    }
  }
}
