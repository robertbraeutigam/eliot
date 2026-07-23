package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

/** The effects-as-channel **weaver** (docs/effects-as-channel.md §6), first slice: under `--effect-channel` with a
  * platform-supplied base carrier, take an effect-blind [[MonomorphicValue]] and produce its woven form
  * ([[WovenValue]]) — the direct-style → monadic elaboration the checker does today, now over concrete post-mono terms.
  *
  * This slice does **carrier assignment + effect-operation resolution** for the Suspend-riding base carrier:
  *   - every abstract *user effect-operation* reference in the body (`Console::printLine`, `Qualifier.Ability`,
  *     machinery excluded — left abstract by the effect-blind desugar/resolver) is resolved to its concrete carrier
  *     instance method at the base carrier, exactly as `PostDrainQuoter.resolveIfAbility` does on the carrier path:
  *     query `AbilityImplementation` at the ability-level carrier argument and emit
  *     `MonomorphicValueReference(implFqn, implTypeArgs)` (e.g. `Console[F~Suspend]::printLine` at `[IO]`);
  *   - an effectful value's signature is wrapped in the carrier (`Unit` ⤳ `IO[Unit]`), so the woven form is the
  *     runnable carrier-headed value the platform entry point expects. The carrier machinery (`Effect`/`Suspend`) and
  *     the effect *instances* keep their carriers (they are not erased), so their carrier-tower bodies monomorphize
  *     normally and need no weaving — only top-level user operations do;
  *   - the platform **entry point** (`main::main`, [[entryPoint]]) is the run boundary: the effect-blind checker types
  *     its body — a bare reference to the user `main` — as pure `Unit`, but the woven user `main` is an `IO[Unit]` that
  *     must be *run*, so the reference is wrapped in the carrier's `runMain` (`eliot.jvm.IO::runMain`), the one node
  *     given a precise carrier-headed type (`IO[Unit]`). This lets a single-operation `Console` program (HelloWorld)
  *     run end-to-end under the flag; the launcher and codegen are unchanged because the entry stays pure `Unit`.
  *
  * It also does **bind/`pure` insertion** for the effectful value's body — the post-mono monadic translation that
  * reintroduces the sequencing the effect-blind checker dropped (see the section comment on `resolveCombinators` below).
  * This includes effects **nested under a pure strict function** (`printLine("a" ++ readLine)`): a strict spine is
  * sequenced whether the effect is on its head or on an argument, and the pure-headed result is `pure`-wrapped.
  *
  * Deferred to later slices (documented so the gaps are visible, not silent): **effectful conditionals** (`fold`/`if`,
  * left structural today — a runtime crash, not a silent wrong answer — because their arms are selected, not all run);
  * precise carrier-headed node types on structurally-woven (non-monadified) sub-terms; control-effect carrier stacks
  * (`weave key = mono key × stack`); and the multi-parameter effect abilities (`State[S, F]`), which this slice's
  * single-carrier-argument query does not yet resolve (they are left abstract rather than mis-resolved).
  *
  * Off the flag, or when no base carrier is configured, the weave is the **identity** image of the `MonomorphicValue`
  * (the carrier path is unchanged).
  */
class WovenValueProcessor(
    effectChannel: Boolean = false,
    baseCarrier: Option[ValueFQN] = None,
    entryPoint: Option[ValueFQN] = None
) extends TransformationProcessor[MonomorphicValue.Key, WovenValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  import WovenValueProcessor.Combinators

  override protected def generateFromKeyAndFact(
      key: WovenValue.Key,
      mv: MonomorphicValue
  ): CompilerIO[WovenValue] =
    baseCarrier.filter(_ => effectChannel) match {
      case Some(carrier) if entryPoint.contains(mv.vfqn) => weaveEntry(mv, carrier).pure[CompilerIO]
      case Some(carrier)                                 => weave(mv, carrier)
      case None                                          =>
        // Default path (effects-as-channel U1, on by default): the identity carrier `Id` and its machinery are erased
        // from the body by [[IdNormalizer]] (docs/effects-as-channel.md §6), so pure code recovers its efficient shape
        // and — with the jvm newtype representation of `Id` ([[GroundValue.carrierFQN]]) — no `Id` allocation ships.
        // Signature and key are unchanged (type/key erasure is the U1b slice). The load-bearing fail-safe: any `Id`
        // machinery the rewrites failed to reach is reported (a U1 warning, a hard error from U4).
        val normalized = mv.runtime.map(IdNormalizer.normalizeValue(mv.vfqn, _))
        warnIdResidue(mv.vfqn, normalized)
          .as(WovenValue(mv.vfqn, mv.typeArguments, mv.name, mv.signature, normalized))
    }

  /** Warn on any `Id` machinery left in a normalized body (the effects-as-channel §6 residue fail-safe — a warning in
    * U1, a hard build error from U4).
    */
  private def warnIdResidue(vfqn: ValueFQN, body: Option[Sourced[MonomorphicExpression.Expression]]): CompilerIO[Unit] =
    body.map(b => IdNormalizer.residualIdReferences(b.value)).filter(_.nonEmpty).traverse_ { residue =>
      warn[CompilerIO](s"effects-as-channel: Id residue survived normalization in ${vfqn.show}: ${residue.map(_.show).mkString(", ")}")
    }

  /** Weave the platform **entry point** (`main::main`, [[entryPoint]]): under the effect-blind checker its body is a
    * bare reference to the user `main`, typed pure `Unit`, but the woven user `main` is an `IO[Unit]` that must be *run*.
    * Wrap that reference in the carrier's run boundary `<carrier module>::runMain` (an ordinary Eliot function
    * `runMain[A](io: IO[A]): A` — the checker can never spell this because it never sees the user `main` as an `IO`).
    * The entry stays pure `Unit` (it runs and returns `Unit`), so codegen and the launcher are unchanged. A body that is
    * not the expected single reference is left un-wrapped (defensive; never a silent mis-weave).
    */
  private def weaveEntry(mv: MonomorphicValue, carrier: ValueFQN): WovenValue =
    WovenValue(mv.vfqn, mv.typeArguments, mv.name, mv.signature, mv.runtime.map(_.map(runBoundary(_, carrier, mv))))

  private def runBoundary(
      body: MonomorphicExpression.Expression,
      carrier: ValueFQN,
      mv: MonomorphicValue
  ): MonomorphicExpression.Expression =
    body match {
      case userMain @ MonomorphicExpression.MonomorphicValueReference(sourcedUserMain, _) =>
        val payload   = mv.signature                          // the entry's return type (`Unit`)
        val ioPayload = carrierApplied(carrier, payload)      // the woven user `main`'s type (`IO[Unit]`)
        val runMainOf = MonomorphicExpression.MonomorphicValueReference(sourcedUserMain.as(runMainFQN(carrier)), Seq(payload))
        MonomorphicExpression.FunctionApplication(
          sourcedUserMain.as(MonomorphicExpression(functionType(ioPayload, payload), runMainOf)),
          sourcedUserMain.as(MonomorphicExpression(ioPayload, userMain))
        )
      case other                                                                          => other
    }

  private def weave(mv: MonomorphicValue, carrier: ValueFQN): CompilerIO[WovenValue] =
    for {
      effectful <- isEffectful(mv.vfqn)
      combos    <- if (effectful) resolveCombinators(carrier) else Option.empty[Combinators].pure[CompilerIO]
      wovenBody <- (effectful, combos) match {
                     // An effectful value's body is direct-style: monadify it (bind/`pure` insertion) so it evaluates to
                     // the carrier. A pure value's body (or an effectful value whose combinators do not resolve — a
                     // non-`Effect`-instanced carrier) is woven structurally, unchanged.
                     case (true, Some(c)) => mv.runtime.traverse(body => weaveMonadicBody(body, mv.signature, c))
                     case _               => mv.runtime.traverse(body => weaveExpression(body.value, carrier).map(body.as))
                   }
      // Only the *return* type is carrier-headed: a parameterized effectful function `shout(s: String): {Console} Unit`
      // wovens to `String -> IO[Unit]`, not `IO[String -> Unit]`. Nullary values (arity 0) wrap their whole signature.
      signature  = if (effectful) carrierReturn(mv.signature, mv.naturalArity.getOrElse(0), carrier) else mv.signature
    } yield WovenValue(mv.vfqn, mv.typeArguments, mv.name, signature, wovenBody)

  // ── Bind/`pure` insertion (the post-mono monadic translation, docs/effects-as-channel.md §6) ──────────────────────
  //
  // The effect-blind checker leaves an effectful value's body in *direct* style: an effectful sub-term (an effect
  // operation, or a call to an effectful value) sits in a position that syntactically expects its plain payload. This
  // translation reintroduces the sequencing the carrier-path `EffectLifter` inserts, but post-monomorphization over
  // concrete terms: an effectful argument to a strict consumer is bound with `flatMap`, and a pure value reaching the
  // carrier is wrapped in `pure`. Both combinators are the carrier's `Effect` instance, resolved at the base carrier
  // exactly like a user effect operation.
  //
  // Scope (kept deliberately narrow so the fail-safe holds — an unhandled shape crashes at runtime, never silently
  // computes the wrong answer): the translation monadifies every *strict* (call-by-value) application — an
  // effect-operation or effectful-function head, a **pure strict head with an effectful argument** (`concat`, so
  // `printLine("a" ++ readLine)` sequences the nested `readLine`), and the block-desugared applied lambda
  // (`val x = e; rest`). The only exception is a **lazy conditional** head (`fold`/`if`), whose arms are selected not
  // all run: it is left structural and `pure`-wrapped so a conditional's arms are never both eagerly run. Effectful
  // conditionals (weaving each arm as a suspended carrier action) are a later slice.

  /** The carrier's `Effect`-instance combinators (`flatMap`/`pure`) resolved at the base carrier — the same resolution a
    * user effect operation gets. `None` if the carrier has no `Effect` instance (then the body is left structural).
    */
  private def resolveCombinators(carrier: ValueFQN): CompilerIO[Option[Combinators]] =
    (resolveOperation(WellKnownTypes.effectFlatMapFQN, carrier), resolveOperation(WellKnownTypes.effectPureFQN, carrier))
      .mapN((flatMap, pure) => (flatMap, pure).mapN(Combinators(carrier, _, _)))

  private def weaveMonadicBody(
      body: Sourced[MonomorphicExpression.Expression],
      signature: GroundValue,
      c: Combinators
  ): CompilerIO[Sourced[MonomorphicExpression.Expression]] =
    peelAndWeave(body, signature, c).runA(0).map(_.map(_.expression))

  /** Peel the leading *parameter* lambdas of an effectful body — they are NOT monadified (a lambda value is pure; its
    * effects happen on application) — descending the signature's arrows in lockstep to the return type. Monadify the
    * inner body at that return type, then rebuild each lambda with a carrier-returning type, so a parameterized
    * effectful function `s -> printLine(s)` wovens to `s -> IO[Unit]`. A nullary body (no leading lambda) is monadified
    * directly, exactly as before.
    */
  private def peelAndWeave(
      expr: Sourced[MonomorphicExpression.Expression],
      returnType: GroundValue,
      c: Combinators
  ): Weave[Sourced[MonomorphicExpression]] =
    expr.value match {
      case MonomorphicExpression.FunctionLiteral(name, parameterType, inner) =>
        peelAndWeave(inner.map(_.expression), functionCodomain(returnType), c).map { wovenInner =>
          expr.as(
            MonomorphicExpression(
              functionType(parameterType, wovenInner.value.expressionType),
              MonomorphicExpression.FunctionLiteral(name, parameterType, wovenInner)
            )
          )
        }
      case _                                                                 =>
        weaveMonadic(expr.map(MonomorphicExpression(returnType, _)), c)
    }

  /** The codomain of a function type (`String -> Unit` ⤳ `Unit`); a non-function type is returned unchanged (defensive —
    * a body with more leading lambdas than the signature has arrows keeps the residual type).
    */
  private def functionCodomain(t: GroundValue): GroundValue =
    t.asFunctionType.map(_._2).getOrElse(t)

  /** Wrap only the *return* type of an effectful value's signature in the carrier, walking `arity` parameter arrows in
    * first: `String -> Unit` at arity 1 becomes `String -> IO[Unit]` (not `IO[String -> Unit]`). At arity 0 this is
    * exactly [[carrierApplied]] (a nullary value's whole signature is its return). Fewer arrows than `arity` (defensive)
    * wraps what remains.
    */
  private def carrierReturn(signature: GroundValue, arity: Int, carrier: ValueFQN): GroundValue =
    if (arity <= 0) carrierApplied(carrier, signature)
    else
      signature.asFunctionType match {
        case Some((parameterType, result)) => functionType(parameterType, carrierReturn(result, arity - 1, carrier))
        case None                          => carrierApplied(carrier, signature)
      }

  /** Translate a node standing in **monadic position** — its value flows into the carrier — into an expression of type
    * `carrier[payload]`, inserting `flatMap`/`pure`. Every result is stamped `carrier[nodePayload]` (the carrier-headed
    * type, so codegen's descriptors line up and no stray casts are emitted).
    */
  private def weaveMonadic(node: Sourced[MonomorphicExpression], c: Combinators): Weave[Sourced[MonomorphicExpression]] =
    node.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, argument) if isLambdaNode(target) =>
        weaveBlock(node, target, argument, c)
      case MonomorphicExpression.FunctionApplication(_, _)                                     =>
        val (head, args) = flattenSpine(node)
        // A *lazy* conditional (`fold`/`if`) selects one arm at runtime, so its arms must NOT be eagerly sequenced;
        // leave it structural (the fail-safe — effectful conditionals are a later slice). Every other spine is a strict
        // (call-by-value) application: if it performs effects anywhere — an effectful head OR an effectful argument
        // nested under a pure strict head (`printLine("a" ++ readLine)`) — sequence it left-to-right.
        if (isLazyConditionalHead(head)) pureWrap(node, c)
        else
          liftW(isEffectfulNode(node)).flatMap {
            case true  => sequenceSpine(node, head, args, c)
            case false => pureWrap(node, c)
          }
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _)                            =>
        liftW(headEffectfulReturning(vfqn.value)).flatMap {
          case true  => liftW(weaveExpression(node.value.expression, c.carrier)).map(io(node, _, c))
          case false => pureWrap(node, c)
        }
      case _                                                                                   => pureWrap(node, c)
    }

  /** A block-desugared applied lambda `(p -> body)(arg)` (`val p = arg; body`): the body runs in monadic position; if
    * `arg` is effectful it is sequenced with `flatMap` (reusing the lambda's own binder), otherwise the value is a plain
    * let-binding.
    */
  private def weaveBlock(
      node: Sourced[MonomorphicExpression],
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression],
      c: Combinators
  ): Weave[Sourced[MonomorphicExpression]] =
    target.value.expression match {
      case MonomorphicExpression.FunctionLiteral(name, parameterType, lambdaBody) =>
        for {
          wovenBody <- weaveMonadic(lambdaBody, c)
          continuation = target.as(
                           MonomorphicExpression(
                             functionType(parameterType, wovenBody.value.expressionType),
                             MonomorphicExpression.FunctionLiteral(name, parameterType, wovenBody)
                           )
                         )
          argEffectful <- liftW(isEffectfulNode(argument))
          result       <- if (argEffectful)
                            weaveMonadic(argument, c).map(action => flatMapApply(node, continuation, action, c))
                          else
                            liftW(weaveExpression(argument.value.expression, c.carrier)).map { wovenArg =>
                              val wovenArgNode = argument.as(MonomorphicExpression(argument.value.expressionType, wovenArg))
                              io(node, MonomorphicExpression.FunctionApplication(continuation, wovenArgNode), c)
                            }
        } yield result
      case _                                                                      => pureWrap(node, c)
    }

  /** A strict application spine `head(a1)…(an)` whose head is effect-op/effectful-returning: bind each effectful
    * argument left-to-right with `flatMap`, then apply the head to the bound (or pure) arguments.
    */
  private def sequenceSpine(
      topNode: Sourced[MonomorphicExpression],
      head: Sourced[MonomorphicExpression],
      args: List[Sourced[MonomorphicExpression]],
      c: Combinators
  ): Weave[Sourced[MonomorphicExpression]] = {
    def go(remaining: List[Sourced[MonomorphicExpression]], bound: List[Sourced[MonomorphicExpression]]): Weave[Sourced[MonomorphicExpression]] =
      remaining match {
        case Nil          => finalApply(topNode, head, bound, c)
        case arg :: rest  =>
          liftW(isEffectfulNode(arg)).flatMap {
            case true  =>
              for {
                action <- weaveMonadic(arg, c)
                name   <- fresh
                boundRef = arg.as(
                             MonomorphicExpression(
                               arg.value.expressionType,
                               MonomorphicExpression.ParameterReference(arg.as(name))
                             )
                           )
                cont   <- go(rest, bound :+ boundRef)
                lambda  = arg.as(
                            MonomorphicExpression(
                              functionType(arg.value.expressionType, cont.value.expressionType),
                              MonomorphicExpression.FunctionLiteral(arg.as(name), arg.value.expressionType, cont)
                            )
                          )
              } yield flatMapApply(topNode, lambda, action, c)
            case false =>
              liftW(weaveExpression(arg.value.expression, c.carrier)).flatMap { wovenArg =>
                go(rest, bound :+ arg.as(MonomorphicExpression(arg.value.expressionType, wovenArg)))
              }
          }
      }

    go(args, Nil)
  }

  /** Apply the spine head to its (bound/pure) arguments. An effect-op/effectful head yields the carrier directly; a pure
    * head's result is `pure`-wrapped; a lambda head (a rarer applied-lambda spine) has its body woven monadically.
    */
  private def finalApply(
      topNode: Sourced[MonomorphicExpression],
      head: Sourced[MonomorphicExpression],
      bound: List[Sourced[MonomorphicExpression]],
      c: Combinators
  ): Weave[Sourced[MonomorphicExpression]] =
    head.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _)  =>
        for {
          headEffectful <- liftW(headEffectfulReturning(vfqn.value))
          wovenHead     <- liftW(weaveExpression(head.value.expression, c.carrier))
          applied        = rebuild(head.as(MonomorphicExpression(head.value.expressionType, wovenHead)), bound)
          result        <- if (headEffectful) io(topNode, applied.value.expression, c).pure[Weave]
                           else pureWrap(topNode.as(MonomorphicExpression(topNode.value.expressionType, applied.value.expression)), c)
        } yield result
      case MonomorphicExpression.FunctionLiteral(name, parameterType, lambdaBody) =>
        weaveMonadic(lambdaBody, c).map { wovenBody =>
          val lambda = head.as(
            MonomorphicExpression(
              head.value.expressionType,
              MonomorphicExpression.FunctionLiteral(name, parameterType, wovenBody)
            )
          )
          io(topNode, rebuild(lambda, bound).value.expression, c)
        }
      case _                                                                      =>
        liftW(weaveExpression(head.value.expression, c.carrier)).flatMap { wovenHead =>
          val applied = rebuild(head.as(MonomorphicExpression(head.value.expressionType, wovenHead)), bound)
          pureWrap(topNode.as(MonomorphicExpression(topNode.value.expressionType, applied.value.expression)), c)
        }
    }

  /** `pure(value)` at the carrier — the pure value woven structurally, wrapped in the resolved `Effect::pure`. */
  private def pureWrap(node: Sourced[MonomorphicExpression], c: Combinators): Weave[Sourced[MonomorphicExpression]] =
    liftW(weaveExpression(node.value.expression, c.carrier)).map { wovenValue =>
      val valueNode = node.as(MonomorphicExpression(node.value.expressionType, wovenValue))
      val pureRef   = node.as(
        MonomorphicExpression(
          functionType(node.value.expressionType, carrierApplied(c.carrier, node.value.expressionType)),
          MonomorphicExpression.MonomorphicValueReference(node.as(c.pure._1), c.pure._2)
        )
      )
      io(node, MonomorphicExpression.FunctionApplication(pureRef, valueNode), c)
    }

  /** `flatMap(continuation)(action)` at the carrier, typed `carrier[topNode payload]`. */
  private def flatMapApply(
      topNode: Sourced[MonomorphicExpression],
      continuation: Sourced[MonomorphicExpression],
      action: Sourced[MonomorphicExpression],
      c: Combinators
  ): Sourced[MonomorphicExpression] = {
    val flatMapRef = topNode.as(
      MonomorphicExpression(GroundValue.Type, MonomorphicExpression.MonomorphicValueReference(topNode.as(c.flatMap._1), c.flatMap._2))
    )
    val partial    = topNode.as(MonomorphicExpression(GroundValue.Type, MonomorphicExpression.FunctionApplication(flatMapRef, continuation)))
    io(topNode, MonomorphicExpression.FunctionApplication(partial, action), c)
  }

  /** Rebuild a (curried) application of `head` to `args`; intermediate node types are cosmetic (the callee's fact drives
    * codegen), so they are left `Type` — the caller re-stamps the final result type.
    */
  private def rebuild(head: Sourced[MonomorphicExpression], args: List[Sourced[MonomorphicExpression]]): Sourced[MonomorphicExpression] =
    args.foldLeft(head)((acc, arg) => acc.as(MonomorphicExpression(GroundValue.Type, MonomorphicExpression.FunctionApplication(acc, arg))))

  /** Stamp `expr` with the carrier-headed type `carrier[node payload]` (`node`'s effect-blind type is the payload). */
  private def io(node: Sourced[MonomorphicExpression], expr: MonomorphicExpression.Expression, c: Combinators): Sourced[MonomorphicExpression] =
    node.as(MonomorphicExpression(carrierApplied(c.carrier, node.value.expressionType), expr))

  /** Whether an expression, evaluated, performs effects: an effect-op/effectful reference, or a spine whose head is such
    * or which has an effectful argument. A lambda *value* is pure (its effects happen on application); literals/params
    * are pure.
    */
  private def isEffectfulNode(node: Sourced[MonomorphicExpression]): CompilerIO[Boolean] =
    node.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => headEffectfulReturning(vfqn.value)
      case MonomorphicExpression.FunctionApplication(_, _)          =>
        val (head, args) = flattenSpine(node)
        (headNodeEffectful(head), args.traverse(isEffectfulNode).map(_.exists(identity))).mapN(_ || _)
      case _                                                        => false.pure[CompilerIO]
    }

  /** Whether the spine head, applied, yields the carrier: an effectful reference, or a lambda whose body is effectful. */
  private def headNodeEffectful(head: Sourced[MonomorphicExpression]): CompilerIO[Boolean] =
    head.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => headEffectfulReturning(vfqn.value)
      case MonomorphicExpression.FunctionLiteral(_, _, body)        => isEffectfulNode(body)
      case MonomorphicExpression.FunctionApplication(_, _)          => isEffectfulNode(head)
      case _                                                        => false.pure[CompilerIO]
    }

  /** Whether a reference returns the carrier: a user effect operation, or a value with a non-empty declared effect row. */
  private def headEffectfulReturning(vfqn: ValueFQN): CompilerIO[Boolean] =
    if (isUserEffectOperation(vfqn)) true.pure[CompilerIO] else isEffectful(vfqn)

  private def isLambdaNode(node: Sourced[MonomorphicExpression]): Boolean =
    node.value.expression match {
      case MonomorphicExpression.FunctionLiteral(_, _, _) => true
      case _                                              => false
    }

  /** Whether a spine head is a *lazy conditional* eliminator (`fold`/`if`, [[WellKnownTypes.boolFoldFQN]] /
    * [[WellKnownTypes.boolIfFQN]]) — the only value-level primitives whose arguments (the arms) are selected rather than
    * all run. They must therefore not be eagerly sequenced like a strict function's effectful arguments; they are left
    * structurally woven here (effectful conditionals — weaving each arm as a suspended carrier action — are a later
    * slice). Every other value is strict (call-by-value), so its effectful arguments are correctly sequenced.
    */
  private def isLazyConditionalHead(head: Sourced[MonomorphicExpression]): Boolean =
    head.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) =>
        vfqn.value == WellKnownTypes.boolFoldFQN || vfqn.value == WellKnownTypes.boolIfFQN
      case _                                                        => false
    }

  @tailrec
  private def flattenSpine(
      node: Sourced[MonomorphicExpression],
      args: List[Sourced[MonomorphicExpression]] = Nil
  ): (Sourced[MonomorphicExpression], List[Sourced[MonomorphicExpression]]) =
    node.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, argument) => flattenSpine(target, argument :: args)
      case _                                                           => (node, args)
    }

  private type Weave[A] = StateT[CompilerIO, Int, A]

  private def liftW[A](io: CompilerIO[A]): Weave[A] = StateT.liftF(io)

  /** A fresh binder name for an inserted `flatMap` continuation (`$weave$N`) — the effect-blind mono body has no such
    * names, so a per-value counter suffices for uniqueness.
    */
  private def fresh: Weave[String] = StateT(n => ((n + 1, s"$$weave$$$n")).pure[CompilerIO])

  /** Rewrite one expression: resolve an abstract user effect-operation reference to its concrete carrier instance
    * method at `carrier`, descending through applications and lambdas. Every other node is preserved (its children still
    * descended). An operation that does not resolve at the single carrier argument (a multi-parameter effect ability, a
    * not-yet-instantiated instance) is left abstract — a later slice, never a silent mis-weave.
    */
  private def weaveExpression(
      expr: MonomorphicExpression.Expression,
      carrier: ValueFQN
  ): CompilerIO[MonomorphicExpression.Expression] = expr match {
    case ref @ MonomorphicExpression.MonomorphicValueReference(vfqn, _) if isUserEffectOperation(vfqn.value) =>
      resolveOperation(vfqn.value, carrier).map {
        case Some((implFqn, implTypeArgs)) => MonomorphicExpression.MonomorphicValueReference(vfqn.as(implFqn), implTypeArgs)
        case None                          => ref
      }
    case MonomorphicExpression.FunctionApplication(target, argument)                                        =>
      (weaveNode(target, carrier), weaveNode(argument, carrier)).mapN(MonomorphicExpression.FunctionApplication.apply)
    case MonomorphicExpression.FunctionLiteral(name, parameterType, body)                                   =>
      weaveNode(body, carrier).map(MonomorphicExpression.FunctionLiteral(name, parameterType, _))
    case other                                                                                              => other.pure[CompilerIO]
  }

  private def weaveNode(
      node: Sourced[MonomorphicExpression],
      carrier: ValueFQN
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    weaveExpression(node.value.expression, carrier).map(woven =>
      node.as(MonomorphicExpression(node.value.expressionType, woven))
    )

  /** Whether a reference is an abstract *user* effect operation — an `Qualifier.Ability` method whose ability is not the
    * carrier machinery. These are exactly the references the effect-blind desugar/resolver left unresolved.
    */
  private def isUserEffectOperation(vfqn: ValueFQN): Boolean =
    EffectMachinery.abilityNameOf(vfqn).exists(name => !EffectMachinery.isMachineryAbility(name))

  /** Resolve a user effect operation at the base carrier: query `AbilityImplementation` at the single ability-level
    * carrier argument (the carrier type constructor), returning the concrete impl method FQN and its impl type
    * arguments — the impl marker's pattern binders bound to ground (e.g. `[IO]` for `Console[F]`). `None` when the
    * operation does not resolve at one carrier argument (a later slice).
    */
  private def resolveOperation(vfqn: ValueFQN, carrier: ValueFQN): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, Seq(carrierConstructor(carrier)), Platform.Runtime))
      .map(_.flatMap(_.resolution.resolved))

  /** Whether the value performs any user effect — read off its declared channel row (`OperatorResolvedValue.effectRow`).
    * An effectful value's woven signature is carrier-headed; a pure value's is unchanged.
    */
  private def isEffectful(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime))
      .map(_.exists(orv => EffectAccountingProcessor.channelDeclaredEffects(orv.effectRow).nonEmpty))

  /** The carrier type constructor unapplied (`IO`), the ability-level argument the effect instance's carrier binder is
    * matched against.
    */
  private def carrierConstructor(carrier: ValueFQN): GroundValue =
    GroundValue.Structure(carrier, Seq.empty, GroundValue.Type)

  /** The carrier applied to a payload (`IO[Unit]`) — the woven signature of an effectful value. */
  private def carrierApplied(carrier: ValueFQN, payload: GroundValue): GroundValue =
    GroundValue.Structure(carrier, Seq(payload), GroundValue.Type)

  /** The arrow type `param -> result` as a ground value (`Structure` headed by the well-known `Function` type
    * constructor, matching `GroundValue.asFunctionType`). Cosmetic on the entry's run-boundary head — codegen derives
    * `runMain`'s descriptors from its own fact, not this node type — but stamped correctly for consistency.
    */
  private def functionType(param: GroundValue, result: GroundValue): GroundValue =
    GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq(param, result), GroundValue.Type)

  /** The carrier's run-boundary function `<carrier module>::runMain` (`eliot.jvm.IO::runMain`): the platform ships it in
    * the carrier's own module, so it is derived from the base-carrier FQN rather than separately configured.
    */
  private def runMainFQN(carrier: ValueFQN): ValueFQN =
    ValueFQN(carrier.moduleName, QualifiedName("runMain", Qualifier.Default))
}

object WovenValueProcessor {

  /** The base carrier's `Effect`-instance combinators used by the monadic translation, each resolved to its concrete
    * impl method FQN and impl type arguments (`[carrier]`) at the base carrier.
    *
    * @param carrier
    *   The base carrier type constructor (e.g. `eliot.jvm.IO::IO`).
    * @param flatMap
    *   The resolved `Effect::flatMap` — `(implFQN, [carrier])`.
    * @param pure
    *   The resolved `Effect::pure` — `(implFQN, [carrier])`.
    */
  private case class Combinators(
      carrier: ValueFQN,
      flatMap: (ValueFQN, Seq[GroundValue]),
      pure: (ValueFQN, Seq[GroundValue])
  )
}
