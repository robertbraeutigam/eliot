package com.vanillasource.eliot.eliotc.saturate.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
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
    saturate(value).map(SaturatedValue.apply)

  /** Fresh generic binder synthesized for one omitted, omittable parameter: its (unique within the value) name and its
    * kind (the referenced parameter's type restriction, e.g. `BigInteger` for `Int`'s bounds).
    */
  private case class FreshBinder(name: String, kind: OperatorResolvedExpression)

  private def saturate(value: OperatorResolvedValue): CompilerIO[OperatorResolvedValue] = {
    val pos = value.name
    rewriteSignature(value.typeStack.value.signature, 0, pos).map { case (newSignature, _, binders) =>
      if (binders.isEmpty) value
      else {
        val existingKind = value.typeStack.value.levels.tail.headOption
          .getOrElse(OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.typeFQN)))
        val newKind      = binders.foldRight(existingKind)((b, acc) => functionType(b.kind, acc, pos))
        val withBinders  = binders.foldRight(newSignature) { (b, acc) =>
          OperatorResolvedExpression.FunctionLiteral(
            pos.as(b.name),
            Some(pos.as(TypeStack.of(b.kind))),
            pos.as(acc)
          )
        }
        // The runtime body's leading value-parameter lambdas still annotate their parameters with the *un*-saturated
        // type (a bare `Int` where the saturated signature now wants `Int[$lo,$hi]`). Strip those annotations: in the
        // body-against-signature check each parameter takes its type from the saturated `VPi` domain instead, which is
        // the authoritative declared type. Capped at the function-type chain depth so a returned-lambda tail is left
        // to infer normally.
        val newRuntime   = value.runtime.map { body =>
          body.as(stripLeadingParamAnnotations(body.value, functionChainDepth(value.typeStack.value.signature)))
        }
        value.copy(
          typeStack = value.typeStack.as(TypeStack(NonEmptySeq.of(withBinders, newKind))),
          runtime = newRuntime
        )
      }
    }
  }

  /** The number of curried `Function[_, _]` links in a signature, after skipping its leading generic binders — i.e. the
    * count of contravariant parameter positions, one per runtime-body value lambda.
    */
  private def functionChainDepth(expr: OperatorResolvedExpression): Int =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(_, _, body) => functionChainDepth(body.value)
      case OperatorResolvedExpression.FunctionApplication(target, cod) =>
        target.value match {
          case OperatorResolvedExpression.FunctionApplication(fn, _) if isFunctionRef(fn.value) =>
            1 + functionChainDepth(cod.value)
          case _                                                                                => 0
        }
      case _                                                      => 0
    }

  /** Drop the parameter-type annotation of the first `count` leading [[OperatorResolvedExpression.FunctionLiteral]]s,
    * stopping early at the first non-lambda. The dropped annotations are redundant with the (saturated) signature.
    */
  private def stripLeadingParamAnnotations(expr: OperatorResolvedExpression, count: Int): OperatorResolvedExpression =
    if (count <= 0) expr
    else
      expr match {
        case OperatorResolvedExpression.FunctionLiteral(paramName, _, body) =>
          OperatorResolvedExpression.FunctionLiteral(
            paramName,
            None,
            body.as(stripLeadingParamAnnotations(body.value, count - 1))
          )
        case other                                                          => other
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
}
