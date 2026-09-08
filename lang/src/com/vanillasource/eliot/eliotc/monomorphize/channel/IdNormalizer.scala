package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-channel **Id-normalization** rewrites (docs/effects-as-channel.md §6), the U1 slice run on by default
  * in the [[WovenValue]] seam. The identity carrier `Id` is the pure carrier of the effects-as-channel design and
  * carries no representation, so its machinery — the `Id` constructor, the `runId` projection, and the `Effect[Id]`
  * combinators `pure`/`flatMap`/`map` — is provably a no-op and is erased here from every monomorphic body before
  * codegen, so pure code recovers its efficient shape and (belt-and-braces with the jvm newtype representation) no `Id`
  * allocation ships.
  *
  * The rewrites, over the concrete post-mono expression tree, each strictly decreasing the `Id`-node count (so the pass
  * is confluent and terminating):
  *   - `runId(e) ⤳ e` — the total, pure projection out of `Id`;
  *   - `Id(e) ⤳ e` — the `Id` value constructor;
  *   - `pure@Effect[Id](e) ⤳ e` — lifting a pure value into `Id` is the value itself;
  *   - `flatMap@Effect[Id](f, m) ⤳ f(m)` and `map@Effect[Id](f, m) ⤳ f(m)` — sequencing on `Id` is application.
  *
  * **Recognition is by FQN, and sanctioned** (docs/effects-as-channel.md §6): `Id`, `runId`, and the `Effect[Id]`
  * combinators are **compiler-owned machinery** the checker inserts by fixed name ([[WellKnownTypes.idFQN]] etc.), not
  * user vocabulary — the ordinary well-known-types practice, unlike the rejected v1 weaver hardcoding of the
  * user-extensible `fold`/`if`. The `Effect[Id]` instance methods are recognised structurally by their module
  * ([[WellKnownTypes.idModuleName]], where `implement Effect[Id]` is colocated with `Id`) and `Effect`-ability
  * qualifier, so the exact canonical pattern string is not hardcoded.
  *
  * **Node types are left unchanged** in this slice (`Id[X]` stays on the nodes — key/type erasure is the U1b slice): an
  * unwrapped node keeps its `Id`-headed type while now holding the payload value, which is sound because the jvm layer
  * gives `Id` a *newtype* representation (`Id[A] ≡ A`), so an `Id[X]`-typed slot and its payload share one machine
  * representation. Any `Id` machinery this pass fails to erase (e.g. a rare first-class combinator reference) therefore
  * still ships as a no-op rather than an allocation.
  */
object IdNormalizer {

  /** Normalize one monomorphic value's runtime body. The **newtype accessor `runId`** is a special case: its own body
    * is the data-accessor `PatternMatch.handleCases` machinery over `Id`, so left intact it would keep `used` pulling
    * in the whole `Id` pattern-match apparatus (the data class, the `handleCases` impl, the selector lambdas) and a
    * first-class `runId` reference would still run that apparatus over an `Id` wrapper the newtype representation no
    * longer allocates — a crash. Because `Id[A] ≡ A`, `runId` **is** the identity, so its body is rewritten to `obj ->
    * obj`; `used` then sees no `handleCases`, the apparatus is never generated, and any `runId` reference (applied or
    * first-class) is a safe identity. Every other value's body gets the ordinary [[normalize]] rewrites.
    */
  def normalizeValue(
      vfqn: ValueFQN,
      signature: GroundValue,
      body: Sourced[MonomorphicExpression.Expression]
  ): Sourced[MonomorphicExpression.Expression] =
    if (vfqn == WellKnownTypes.runIdFQN) identityAccessorBody(body) else normalize(body, signature)

  /** Normalize the body expression of one monomorphic value, applying the `Id` rewrites bottom-up. The value's runtime
    * body is a `Sourced` *untyped* top-level expression (its children carry types); it is bridged to the typed
    * [[normalizeNode]] with `topType` as the top node's type — the value's signature, so that a first-class
    * `Id`-machinery reference standing as the *whole body* (`def r = runId`) still carries the function type its
    * eta-expansion needs. (For every non-leaf top node the type is otherwise discarded; each child carries its own
    * type.)
    */
  def normalize(
      body: Sourced[MonomorphicExpression.Expression],
      topType: GroundValue = GroundValue.Type
  ): Sourced[MonomorphicExpression.Expression] =
    normalizeNode(body.map(MonomorphicExpression(topType, _))).map(_.expression)

  /** Rewrite the single-parameter data-accessor body `obj -> handleCases(obj){ Id(x) -> x }` to the identity `obj ->
    * obj`. A body that is not the expected single lambda is left as-is (defensive — never a silent mis-rewrite).
    */
  private def identityAccessorBody(
      body: Sourced[MonomorphicExpression.Expression]
  ): Sourced[MonomorphicExpression.Expression] =
    body.map {
      case MonomorphicExpression.FunctionLiteral(name, parameterType, inner) =>
        MonomorphicExpression.FunctionLiteral(
          name,
          parameterType,
          inner.map(me => MonomorphicExpression(me.expressionType, MonomorphicExpression.ParameterReference(name)))
        )
      case other                                                             => other
    }

  /** Normalize one node, rewriting an `Id`-machinery application at its root and recursing into what remains. A dropped
    * wrapper (`runId`/`Id`/`pure@Effect[Id]`) yields its (normalized) argument node *with the argument's own type* —
    * the outer `Id`-headed type is discarded, which the newtype representation makes representationally identical.
    */
  private def normalizeNode(node: Sourced[MonomorphicExpression]): Sourced[MonomorphicExpression] =
    node.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, argument) if isDropWrapperRef(target) =>
        normalizeNode(argument)
      case MonomorphicExpression.FunctionApplication(target, argument)                             =>
        target.value.expression match {
          case MonomorphicExpression.FunctionApplication(comboHead, f) if isApplyCombinatorRef(comboHead) =>
            retype(node, MonomorphicExpression.FunctionApplication(normalizeNode(f), normalizeNode(argument)))
          case _                                                                                          =>
            retype(node, MonomorphicExpression.FunctionApplication(normalizeNode(target), normalizeNode(argument)))
        }
      case MonomorphicExpression.FunctionLiteral(name, parameterType, innerBody)                   =>
        retype(node, MonomorphicExpression.FunctionLiteral(name, parameterType, normalizeNode(innerBody)))
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) if isIdMachinery(vfqn.value)   =>
        etaExpand(node, vfqn.value)
      case _                                                                                       => node
    }

  private def retype(
      node: Sourced[MonomorphicExpression],
      expression: MonomorphicExpression.Expression
  ): Sourced[MonomorphicExpression] =
    node.map(me => MonomorphicExpression(me.expressionType, expression))

  // ── Eta-expansion of a first-class `Id`-machinery reference (docs/effects-as-channel.md §6) ───────────────────────
  //
  // The rewrites above collapse *applied* `Id` machinery. A **first-class** reference — the combinator passed as a
  // function value, e.g. `runId` reached through a dot-chain `x.runId` (which lowers to `_dot_(x, runId)`) — reaches
  // this node as a bare `MonomorphicValueReference`. Left alone it would still ship an `Id` reference (the residue
  // fail-safe warns on it; the newtype keeps it a no-op). Eta-expanding it to the equivalent lambda removes the last
  // `Id` reference, so post-normalization no `Id` machinery survives at all. Since `Id[A] ≡ A`, the equivalents are:
  // `runId`/`Id`/`pure@Effect[Id]` (arity 1) ⤳ the identity `x -> x`; `flatMap`/`map@Effect[Id]` (arity 2) ⤳ the
  // application `f -> m -> f(m)`. The lambda is built from the reference's own (still `Id`-typed) function type; the
  // later U1b `eraseIdInBody` pass erases those `Id` types like any other.

  private def etaExpand(node: Sourced[MonomorphicExpression], fqn: ValueFQN): Sourced[MonomorphicExpression] =
    if (isEffectIdMethod(fqn, "flatMap") || isEffectIdMethod(fqn, "map")) etaApply(node)
    else etaIdentity(node)

  /** `ref: P -> R` ⤳ `x -> x` (`x` bound at `P`, referenced at `R`; `P ≡ R` under the newtype). */
  private def etaIdentity(node: Sourced[MonomorphicExpression]): Sourced[MonomorphicExpression] =
    node.value.expressionType.asFunctionType match {
      case Some((paramType, resultType)) =>
        val x = node.as("$idEta")
        node.as(
          MonomorphicExpression(
            node.value.expressionType,
            MonomorphicExpression.FunctionLiteral(
              x,
              paramType,
              node.as(MonomorphicExpression(resultType, MonomorphicExpression.ParameterReference(x)))
            )
          )
        )
      case None                          => node
    }

  /** `ref: F -> M -> R` ⤳ `f -> m -> f(m)` (`f` bound at the continuation type `F`, `m` at `M`, `f(m)` at `R`). */
  private def etaApply(node: Sourced[MonomorphicExpression]): Sourced[MonomorphicExpression] =
    (for {
      (fType, rest)       <- node.value.expressionType.asFunctionType
      (mType, resultType) <- rest.asFunctionType
    } yield {
      val f       = node.as("$idEtaFn")
      val m       = node.as("$idEtaArg")
      val applied = node.as(
        MonomorphicExpression(
          resultType,
          MonomorphicExpression.FunctionApplication(
            node.as(MonomorphicExpression(fType, MonomorphicExpression.ParameterReference(f))),
            node.as(MonomorphicExpression(mType, MonomorphicExpression.ParameterReference(m)))
          )
        )
      )
      val inner   = node.as(MonomorphicExpression(rest, MonomorphicExpression.FunctionLiteral(m, mType, applied)))
      node.as(MonomorphicExpression(node.value.expressionType, MonomorphicExpression.FunctionLiteral(f, fType, inner)))
    }).getOrElse(node)

  // ── U1b: type/key erasure (`Id[X] ⤳ X`, docs/effects-as-channel.md §6/§10) ───────────────────────────────────────
  //
  // After the body rewrites (U1a) an `Id[X]` node is representationally its payload; U1b makes that explicit in the
  // *types* too — every `Id`-headed type erases to its payload, in node types, signatures, and the type arguments a
  // callee is instantiated at. Because a reference's erased type arguments become the callee's demanded mono key, this
  // is what *merges* an `Id`-instantiation with its payload instantiation (`fold[Id[String]]` and `fold[String]` become
  // one demand, one generated method). Only a *bare* `Id` (unapplied, as a higher-kinded carrier argument like the `G`
  // of `AbortCarrier[Id, A]`) is left — it has no payload to collapse to and its carrier already erases to its own head.

  /** Erase every `Id`-headed type to its payload, recursively (`Id[X] ⤳ X`, `Box[Id[X]] ⤳ Box[X]`). A bare unapplied
    * `Id` (no payload) is left unchanged.
    */
  def eraseIdTypes(gv: GroundValue): GroundValue =
    gv match {
      case GroundValue.Structure(name, arg +: _, _) if name == WellKnownTypes.idFQN => eraseIdTypes(arg)
      case GroundValue.Structure(name, args, valueType)                             =>
        GroundValue.Structure(name, args.map(eraseIdTypes), eraseIdTypes(valueType))
      case GroundValue.Param(index, args, valueType)                                =>
        GroundValue.Param(index, args.map(eraseIdTypes), eraseIdTypes(valueType))
      case GroundValue.Direct(value, valueType)                                     =>
        GroundValue.Direct(value, eraseIdTypes(valueType))
      case GroundValue.Type                                                         => GroundValue.Type
    }

  /** Erase `Id`-headed types throughout a body: every node's `expressionType`, every function-literal parameter type,
    * and every value reference's type arguments (which are the callee's demanded mono key — this is where the
    * Id-instantiation merge happens).
    */
  def eraseIdInBody(body: Sourced[MonomorphicExpression.Expression]): Sourced[MonomorphicExpression.Expression] =
    eraseIdInNode(body.map(MonomorphicExpression(GroundValue.Type, _))).map(_.expression)

  private def eraseIdInNode(node: Sourced[MonomorphicExpression]): Sourced[MonomorphicExpression] =
    node.map { me =>
      val erased = me.expression match {
        case MonomorphicExpression.FunctionApplication(target, argument)       =>
          MonomorphicExpression.FunctionApplication(eraseIdInNode(target), eraseIdInNode(argument))
        case MonomorphicExpression.FunctionLiteral(name, parameterType, inner) =>
          MonomorphicExpression.FunctionLiteral(name, eraseIdTypes(parameterType), eraseIdInNode(inner))
        case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)   =>
          MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs.map(eraseIdTypes))
        case other                                                             => other
      }
      MonomorphicExpression(eraseIdTypes(me.expressionType), erased)
    }

  /** Any `Id`-machinery *references* still present in a (normalized) body — the residue the load-bearing fail-safe
    * reports (docs/effects-as-channel.md §6): a warning during U1 bring-up, a hard error from U4. A non-empty result
    * means a body shape [[normalize]] did not reach (e.g. a rare first-class combinator reference); the jvm newtype
    * representation of `Id` still keeps such residue a no-op rather than an allocation, which is why U1 only warns.
    * Node *types* are intentionally excluded — `Id[X]` legitimately survives on nodes until the U1b type/key erasure
    * slice.
    */
  def residualIdReferences(body: MonomorphicExpression.Expression): Seq[ValueFQN] =
    collectReferences(body).filter(isIdMachinery)

  /** Whether any `Id`-headed type (`Id[X]`) survives in a value's signature or body after U1b type erasure — the type
    * half of the residue fail-safe. A *bare* `Id` (unapplied carrier marker, e.g. the `G` of `AbortCarrier[Id, A]`) is
    * not flagged: it has no payload to erase and legitimately survives until deeper stack lowering.
    */
  def hasResidualIdType(
      signature: GroundValue,
      body: Option[Sourced[MonomorphicExpression.Expression]]
  ): Boolean =
    typeHasIdHead(signature) || body.exists(b => bodyTypes(b.value).exists(typeHasIdHead))

  private def typeHasIdHead(gv: GroundValue): Boolean =
    gv match {
      case GroundValue.Structure(name, arg +: _, _) if name == WellKnownTypes.idFQN => true
      case GroundValue.Structure(_, args, valueType)                                => (args :+ valueType).exists(typeHasIdHead)
      case GroundValue.Param(_, args, valueType)                                    => (args :+ valueType).exists(typeHasIdHead)
      case GroundValue.Direct(_, valueType)                                         => typeHasIdHead(valueType)
      case GroundValue.Type                                                         => false
    }

  private def bodyTypes(expr: MonomorphicExpression.Expression): Seq[GroundValue] =
    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument)       =>
        nodeTypes(target) ++ nodeTypes(argument)
      case MonomorphicExpression.FunctionLiteral(_, parameterType, inner)    => parameterType +: nodeTypes(inner)
      case MonomorphicExpression.MonomorphicValueReference(_, typeArguments) => typeArguments
      case _                                                                 => Seq.empty
    }

  private def nodeTypes(node: Sourced[MonomorphicExpression]): Seq[GroundValue] =
    node.value.expressionType +: bodyTypes(node.value.expression)

  private def isIdMachinery(fqn: ValueFQN): Boolean =
    fqn == WellKnownTypes.runIdFQN || fqn == WellKnownTypes.idConstructorFQN ||
      isEffectIdMethod(fqn, "pure") || isEffectIdMethod(fqn, "flatMap") || isEffectIdMethod(fqn, "map")

  private def collectReferences(expr: MonomorphicExpression.Expression): Seq[ValueFQN] =
    expr match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _)    => Seq(vfqn.value)
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        collectReferences(target.value.expression) ++ collectReferences(argument.value.expression)
      case MonomorphicExpression.FunctionLiteral(_, _, innerBody)      => collectReferences(innerBody.value.expression)
      case _                                                           => Seq.empty
    }

  /** A single-argument `Id` wrapper whose application is dropped (`runId(e)`/`Id(e)`/`pure@Effect[Id](e) ⤳ e`). */
  private def isDropWrapperRef(node: Sourced[MonomorphicExpression]): Boolean =
    headRef(node).exists { case (fqn, typeArgs) =>
      fqn == WellKnownTypes.runIdFQN || fqn == WellKnownTypes.idConstructorFQN ||
      isEffectIdMethod(fqn, "pure") || isAbstractEffectIdCombinator(fqn, typeArgs, WellKnownTypes.effectPureFQN)
    }

  /** A two-argument `Effect[Id]` combinator whose application collapses to plain application (`flatMap@Effect[Id](f,
    * m)`/`map@Effect[Id](f, m) ⤳ f(m)`).
    */
  private def isApplyCombinatorRef(node: Sourced[MonomorphicExpression]): Boolean =
    headRef(node).exists { case (fqn, typeArgs) =>
      isEffectIdMethod(fqn, "flatMap") || isEffectIdMethod(fqn, "map") ||
      isAbstractEffectIdCombinator(fqn, typeArgs, WellKnownTypes.effectFlatMapFQN) ||
      isAbstractEffectIdCombinator(fqn, typeArgs, WellKnownTypes.effectMapFQN)
    }

  private def headRef(node: Sourced[MonomorphicExpression]): Option[(ValueFQN, Seq[GroundValue])] =
    node.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) => Some((vfqn.value, typeArgs))
      case _                                                               => None
    }

  /** The **abstract** (unresolved) form of an `Effect[Id]` combinator: `pure`/`flatMap`/`map` still on the
    * `eliot.carrier.Effect` ability-method FQN (`abstractFqn`, e.g. [[WellKnownTypes.effectPureFQN]]) rather than the
    * resolved `Id` impl, recognised by its leading carrier type-argument being `Id`. Ability resolution normally
    * rewrites these to the impl (handled by [[isEffectIdMethod]]); the abstract form survives only where no
    * `Effect[Id]` instance is in scope (a unit-test stub) or on a resolution gap. Recognising it too makes the `Id`
    * erasure **resolution-agnostic** — the same checker-inserted machinery, whether resolved or not — mirroring the
    * `SemExpression`-level strip in `PostDrainQuoter`; being still the identity, erasing it is always sound. Firing is
    * guarded by the `Id` carrier arg, so an `Effect[C]` combinator over any real carrier is never touched.
    */
  private def isAbstractEffectIdCombinator(fqn: ValueFQN, typeArgs: Seq[GroundValue], abstractFqn: ValueFQN): Boolean =
    fqn == abstractFqn && typeArgs.headOption.exists {
      case GroundValue.Structure(name, _, _) => name == WellKnownTypes.idFQN
      case _                                 => false
    }

  /** Whether `fqn` is the named method of the `Effect[Id]` instance — recognised by living in the `Id` module (where
    * `implement Effect[Id]` is colocated with `Id`) with the `Effect` ability-implementation qualifier and the given
    * method name.
    */
  private def isEffectIdMethod(fqn: ValueFQN, method: String): Boolean =
    fqn.moduleName == WellKnownTypes.idModuleName && fqn.name.name == method &&
      (fqn.name.qualifier match {
        case Qualifier.AbilityImplementation("Effect", _) => true
        case _                                            => false
      })
}
