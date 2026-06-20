package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.fact.EffectDesugaredValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionApplication,
  FunctionLiteral,
  IntegerLiteral,
  ParameterReference,
  SignatureView,
  StringLiteral,
  ValueReference,
  applyChain,
  arrow,
  asArrow,
  isFunctionReference,
  spine
}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Effects, Part B — the body auto-lift (the headline of the effects plan, M3).
  *
  * Rewrites a value's runtime body from direct style into ordinary monadic form, so a developer writes
  * `def echo : {Console} Unit = println(readLine)` and the compiler produces `flatMap(readLine, x -> println(x))`. The
  * single rule: an effectful sub-term (one whose static result is headed by an effect carrier) flowing into a *pure
  * value-argument position* is sequenced with the carrier's `Monad.flatMap` (or `Applicative.map`, when the
  * continuation is pure), binding it to a fresh variable; a pure body under an effectful carrier return is wrapped with
  * `Monad.pure`. There is no `lift`: every effectful sub-term shares the one carrier, so there are never layers to
  * cross inside a body.
  *
  * "Effectful" is decided structurally from operator-resolved signatures (this phase runs before monomorphization,
  * which remains the sole arbiter and backstop):
  *   - a callee's result is effectful iff, once its value parameters are applied, the result type is headed by one of
  *     the callee's own higher-kinded generic binders (its carrier) — e.g. `readLine : F[String]`, `println(s) : F[Unit]`;
  *   - a parameter reference is effectful iff its declared type is headed by one of the *current value's* carrier
  *     binders — e.g. `fa : F[String]`, so `println(fa)` lifts but `id(x) = x` does not.
  *
  * A position auto-binds only when the callee's parameter there is a concrete (non-`Function`) named type — never a
  * carrier-typed parameter (`fa : F[A]`, a storage position, as in `flatMap`'s first argument) nor a function-typed or
  * polymorphic parameter — so already-monadic code passes through unchanged (the rewrite is idempotent) and a stored
  * effect action is not bound. Multiple effectful arguments bind left to right.
  *
  * Fail-safe: a value with no effect carrier (a nullary return such as `Unit`) whose body nevertheless performs an
  * effect is rejected here ("performs effects but its return type cannot carry them") rather than miscompiled;
  * monomorphization would also catch it as an `F[A]`-vs-`A` mismatch.
  *
  * Placed after `OperatorResolverProcessor` (so application structure is final) and before `SaturatedValueProcessor`
  * (whose primary input is repointed to this fact); the signature is untouched, so saturate's cross-value reads of
  * other [[OperatorResolvedValue]] signatures stay valid.
  */
class EffectDesugaringProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, EffectDesugaredValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  import EffectDesugaringProcessor.*

  override protected def generateFromKeyAndFact(
      key: EffectDesugaredValue.Key,
      value: OperatorResolvedValue
  ): CompilerIO[EffectDesugaredValue] =
    value.runtime match {
      case None       => EffectDesugaredValue(value).pure[CompilerIO]
      case Some(body) => desugarBody(value, body).map(runtime => EffectDesugaredValue(value.copy(runtime = runtime)))
    }

  /** Auto-lift one value's body against its declared signature, returning the rewritten runtime (or registering a
    * fail-safe error and returning the original, which then will not be registered downstream).
    */
  private def desugarBody(
      value: OperatorResolvedValue,
      body: Sourced[OperatorResolvedExpression]
  ): CompilerIO[Option[Sourced[OperatorResolvedExpression]]] = {
    val view                  = SignatureView.of(value.typeStack.as(value.typeStack.value.signature))
    // The value's own ambient effect carrier(s): a higher-kinded binder that carries an ability constraint — the M1
    // `{E...}` carrier (`[F[_] ~ E...]`) or a hand-written `[F[_] ~ Monad]`. A bare higher-kinded generic (`C[_, _]`
    // in `f[A, B, C[_, _]]`) is NOT a carrier, so a body like `id(c)` is never spuriously `pure`-wrapped.
    val carrier               =
      view.binders.filter(b => isHktBinder(b) && value.paramConstraints.contains(b.name.value)).map(_.name.value).toSet

    // The number of value parameters is the count of leading lambdas the *body* actually has, NOT the arrow-arity of
    // the declared type: a function-*valued* def (`def f : Function[A, B] = g`) has 0 parameters but a 2-arrow type, so
    // `SignatureView` would over-decompose its return to `B`. The effective return the inner body must produce is the
    // declared type with only the peeled parameters' arrows removed. The body's leading value-parameter lambdas are
    // unannotated (the type stack is the single source of truth), so each peeled parameter's type comes from the view.
    val (paramNames, inner)   = peelParameters(body, view.parameters.size)
    val env                   = paramNames.map(_.value).zip(view.parameters.map(_.value)).toMap
    val effectiveReturn       =
      view.parameters.drop(paramNames.size).foldRight(view.returnType)((dom, cod) => cod.as(arrow(dom, cod)))
    val (retHead, retArgs)    = spine(effectiveReturn.value)
    val returnIsCarrierBinder = retHead match {
      case ParameterReference(n) => carrier.contains(n.value)
      case _                     => false
    }
    // A nullary return type (`Unit`, `String`) cannot host effects; an applied one (`F[Unit]`, `IO[Unit]`, `Box[A]`)
    // can — only an effectful body in the former case is the "declared pure" error.
    val canHostEffects        = retArgs.nonEmpty

    desugar(inner, env, carrier, 0).flatMap { case Desugared(newInner, effectful, _) =>
      val finalInner =
        if (effectful && !canHostEffects)
          compilerError(
            value.name.as(
              "This value performs an effect but is declared pure; declare an effect set with { ... } or return an " +
                "effect carrier."
            )
          ).as(newInner)
        else if (!effectful && returnIsCarrierBinder) pureWrap(newInner).pure[CompilerIO]
        else newInner.pure[CompilerIO]

      finalInner.map(fi => Some(rewrapParameters(body, paramNames, fi)))
    }
  }

  /** Peel `count` leading value-parameter [[FunctionLiteral]]s off a body, returning their names and the inner body. */
  private def peelParameters(
      body: Sourced[OperatorResolvedExpression],
      count: Int
  ): (Seq[Sourced[String]], Sourced[OperatorResolvedExpression]) =
    if (count <= 0) (Seq.empty, body)
    else
      body.value match {
        case FunctionLiteral(name, _, innerBody) =>
          val (rest, inner) = peelParameters(innerBody, count - 1)
          (name +: rest, inner)
        case _                                   => (Seq.empty, body)
      }

  /** Re-attach the peeled value-parameter lambdas (unannotated, as they were) around a rewritten inner body. */
  private def rewrapParameters(
      original: Sourced[OperatorResolvedExpression],
      paramNames: Seq[Sourced[String]],
      inner: Sourced[OperatorResolvedExpression]
  ): Sourced[OperatorResolvedExpression] =
    paramNames.foldRight(inner)((name, acc) => original.as(FunctionLiteral(name, None, acc)))

  /** The recursive bottom-up auto-lift over one expression. `env` maps in-scope value parameters to their declared
    * types; `carrier` is the current value's higher-kinded carrier binder names; `idx` threads fresh-variable numbering.
    */
  private def desugar(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  ): CompilerIO[Desugared] =
    expr.value match {
      case _: IntegerLiteral | _: StringLiteral =>
        Desugared(expr, effectful = false, idx).pure[CompilerIO]
      case ParameterReference(name)             =>
        Desugared(expr, effectful = env.get(name.value).exists(carrierHeaded(_, carrier)), idx).pure[CompilerIO]
      case ValueReference(fqn, _)               =>
        calleeInfo(fqn).map(info => Desugared(expr, resultEffectful(info, 0), idx))
      case FunctionLiteral(name, paramType, lambdaBody)  =>
        desugar(lambdaBody, env - name.value, carrier, idx).map { case Desugared(newBody, _, idx2) =>
          // The lambda *value* is pure (a function); its body may be effectful (e.g. a `flatMap` continuation).
          Desugared(expr.as(FunctionLiteral(name, paramType, newBody)), effectful = false, idx2)
        }
      case _: FunctionApplication                        =>
        desugarApplication(expr, env, carrier, idx)
    }

  private def desugarApplication(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  ): CompilerIO[Desugared] = {
    val (head, args) = sourcedSpine(expr)
    head.value match {
      case ValueReference(fqn, _) =>
        for {
          info               <- calleeInfo(fqn)
          (argResults, idxA) <- args.foldLeftM((Seq.empty[Desugared], idx)) { case ((acc, i), arg) =>
                                  desugar(arg, env, carrier, i).map(r => (acc :+ r, r.nextIdx))
                                }
          (coreArgs, binds, idxB) = buildArguments(info, argResults, idxA)
          core                    = expr.as(applyChain(head, coreArgs))
          coreEffectful           = resultEffectful(info, args.size)
        } yield wrapBinds(core, coreEffectful, binds, idxB)
      case _                      =>
        // Non-value-reference head (immediately-applied lambda, applied parameter): no signature to read, so rebuild
        // structurally without binding and let monomorphization arbitrate. Conservative, fail-safe.
        for {
          headRes           <- desugar(head, env, carrier, idx)
          (argExprs, idxA)  <- args.foldLeftM((Seq.empty[Sourced[OperatorResolvedExpression]], headRes.nextIdx)) {
                                 case ((acc, i), arg) => desugar(arg, env, carrier, i).map(r => (acc :+ r.expr, r.nextIdx))
                               }
        } yield Desugared(expr.as(applyChain(headRes.expr, argExprs)), effectful = false, idxA)
    }
  }

  /** Decide, per argument, whether to bind it (an effectful argument flowing into a concrete value position) or pass it
    * through, minting a fresh variable for each bind. Returns the (possibly substituted) core arguments, the binds in
    * left-to-right order, and the next fresh index.
    */
  private def buildArguments(
      info: CalleeInfo,
      argResults: Seq[Desugared],
      idx: Int
  ): (Seq[Sourced[OperatorResolvedExpression]], Seq[Bind], Int) =
    argResults.zipWithIndex.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[Bind], idx)) {
      case ((coreArgs, binds, i), (arg, pos)) =>
        if (arg.effectful && isBindPosition(info, pos)) {
          val name = freshName(i)
          (coreArgs :+ arg.expr.as(ParameterReference(arg.expr.as(name))), binds :+ Bind(name, arg.expr), i + 1)
        } else (coreArgs :+ arg.expr, binds, i)
    }

  /** Fold the binds around the core, innermost first: the bind nearest the core uses `map` if the core is pure (lifting
    * it into the carrier) and `flatMap` if it is effectful; every outer bind wraps an effectful continuation, so it is
    * always `flatMap`. With no binds the core is returned with its own effectfulness.
    */
  private def wrapBinds(
      core: Sourced[OperatorResolvedExpression],
      coreEffectful: Boolean,
      binds: Seq[Bind],
      idx: Int
  ): Desugared =
    if (binds.isEmpty) Desugared(core, coreEffectful, idx)
    else {
      val (folded, _) = binds.foldRight((core, coreEffectful)) { case (bind, (acc, accEffectful)) =>
        val pos        = bind.action
        val lambda     = pos.as(FunctionLiteral(pos.as(bind.name), None, acc))
        val combinator = pos.as(ValueReference(pos.as(if (accEffectful) flatMapFQN else mapFQN)))
        (pos.as(applyChain(combinator, Seq(bind.action, lambda))), true)
      }
      Desugared(folded, effectful = true, idx)
    }

  /** Lift a pure body into the carrier with `Monad.pure`. */
  private def pureWrap(expr: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] =
    expr.as(applyChain(expr.as(ValueReference(expr.as(pureFQN))), Seq(expr)))

  /** Read a callee's operator-resolved signature into the shape the auto-lift needs: its value-parameter types, its
    * return type, and the names of its higher-kinded (carrier) generic binders. An unresolvable callee is treated as a
    * zero-parameter pure value (conservative; monomorphization backstops).
    */
  private def calleeInfo(fqn: Sourced[ValueFQN]): CompilerIO[CalleeInfo] =
    getFact(OperatorResolvedValue.Key(fqn.value)).map {
      case Some(orv) =>
        val view = SignatureView.of(orv.typeStack.as(orv.typeStack.value.signature))
        CalleeInfo(
          view.parameters.map(_.value),
          view.returnType.value,
          view.binders.filter(isHktBinder).map(_.name.value).toSet
        )
      case None      =>
        CalleeInfo(Seq.empty, ValueReference(fqn.as(WellKnownTypes.typeFQN)), Set.empty)
    }

  /** Whether a callee applied to `appliedCount` value arguments yields an effectful result: it must be fully applied and
    * its result type headed by one of the callee's own carrier binders.
    */
  private def resultEffectful(info: CalleeInfo, appliedCount: Int): Boolean =
    appliedCount >= info.valueParamTypes.size && (spine(info.returnType)._1 match {
      case ParameterReference(n) => info.carrierBinders.contains(n.value)
      case _                     => false
    })

  /** Whether argument position `pos` auto-binds an effectful argument: the callee's parameter there must be a concrete,
    * non-`Function` named type. Carrier-typed (`F[A]`, a [[ParameterReference]] head) and function-typed parameters are
    * storage positions that take the effectful value directly.
    */
  private def isBindPosition(info: CalleeInfo, pos: Int): Boolean =
    info.valueParamTypes.lift(pos).exists { paramType =>
      spine(paramType)._1 match {
        case ref: ValueReference => !isFunctionReference(ref)
        case _                   => false
      }
    }
}

object EffectDesugaringProcessor {
  private val monadModule       = ModuleName(ModuleName.defaultSystemPackage, "Monad")
  private val applicativeModule = ModuleName(ModuleName.defaultSystemPackage, "Applicative")

  /** The effect machinery is referenced by fully-qualified name (never imported by the user): `Monad.flatMap`/`pure`
    * sequence and lift, `Applicative.map` transforms a pure continuation.
    */
  val flatMapFQN: ValueFQN = ValueFQN(monadModule, QualifiedName("flatMap", Qualifier.Ability("Monad")))
  val pureFQN: ValueFQN    = ValueFQN(monadModule, QualifiedName("pure", Qualifier.Ability("Monad")))
  val mapFQN: ValueFQN     = ValueFQN(applicativeModule, QualifiedName("map", Qualifier.Ability("Applicative")))

  /** One pending sequencing point: the effectful action and the fresh variable its result binds to. */
  private case class Bind(name: String, action: Sourced[OperatorResolvedExpression])

  /** A desugared sub-expression: the rewritten expression, whether its static result is effectful, and the next fresh
    * variable index (threaded so synthesized binders are unique within the body).
    */
  private case class Desugared(expr: Sourced[OperatorResolvedExpression], effectful: Boolean, nextIdx: Int)

  /** A callee's signature reduced to what the auto-lift reads. */
  private case class CalleeInfo(
      valueParamTypes: Seq[OperatorResolvedExpression],
      returnType: OperatorResolvedExpression,
      carrierBinders: Set[String]
  )

  private def freshName(idx: Int): String = s"$$eff$$$idx"

  /** A generic binder is a carrier iff its kind is an arrow (`Type -> Type`, i.e. higher-kinded). */
  private def isHktBinder(binder: SignatureView.Binder): Boolean =
    binder.parameterType.exists(pt => asArrow(pt.value.signature).isDefined)

  /** Whether a type expression is headed by one of `carrier` (a carrier-typed value, e.g. `F[String]`). */
  private def carrierHeaded(tpe: OperatorResolvedExpression, carrier: Set[String]): Boolean =
    spine(tpe)._1 match {
      case ParameterReference(n) => carrier.contains(n.value)
      case _                     => false
    }

  /** Decompose an application into its head (keeping its source position) and left-to-right arguments. */
  private def sourcedSpine(
      expr: Sourced[OperatorResolvedExpression]
  ): (Sourced[OperatorResolvedExpression], Seq[Sourced[OperatorResolvedExpression]]) =
    expr.value match {
      case FunctionApplication(target, arg) =>
        val (head, args) = sourcedSpine(target)
        (head, args :+ arg)
      case _                                => (expr, Seq.empty)
    }
}
