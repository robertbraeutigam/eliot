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

  /** Normalize one monomorphic value's runtime body. The **newtype accessor `runId`** is a special case: its own body is
    * the data-accessor `PatternMatch.handleCases` machinery over `Id`, so left intact it would keep `used` pulling in the
    * whole `Id` pattern-match apparatus (the data class, the `handleCases` impl, the selector lambdas) and a first-class
    * `runId` reference would still run that apparatus over an `Id` wrapper the newtype representation no longer allocates
    * — a crash. Because `Id[A] ≡ A`, `runId` **is** the identity, so its body is rewritten to `obj -> obj`; `used` then
    * sees no `handleCases`, the apparatus is never generated, and any `runId` reference (applied or first-class) is a
    * safe identity. Every other value's body gets the ordinary [[normalize]] rewrites.
    */
  def normalizeValue(vfqn: ValueFQN, body: Sourced[MonomorphicExpression.Expression]): Sourced[MonomorphicExpression.Expression] =
    if (vfqn == WellKnownTypes.runIdFQN) identityAccessorBody(body) else normalize(body)

  /** Normalize the body expression of one monomorphic value, applying the `Id` rewrites bottom-up. The value's runtime
    * body is a `Sourced` *untyped* top-level expression (its children carry types); it is bridged to the typed
    * [[normalizeNode]] with a placeholder top type whose only use is discarded — the top node's type is never read (the
    * value's signature carries the return type, and each child carries its own).
    */
  def normalize(body: Sourced[MonomorphicExpression.Expression]): Sourced[MonomorphicExpression.Expression] =
    normalizeNode(body.map(MonomorphicExpression(GroundValue.Type, _))).map(_.expression)

  /** Rewrite the single-parameter data-accessor body `obj -> handleCases(obj){ Id(x) -> x }` to the identity
    * `obj -> obj`. A body that is not the expected single lambda is left as-is (defensive — never a silent mis-rewrite).
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
    * wrapper (`runId`/`Id`/`pure@Effect[Id]`) yields its (normalized) argument node *with the argument's own type* — the
    * outer `Id`-headed type is discarded, which the newtype representation makes representationally identical.
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
      case _                                                                                       => node
    }

  private def retype(
      node: Sourced[MonomorphicExpression],
      expression: MonomorphicExpression.Expression
  ): Sourced[MonomorphicExpression] =
    node.map(me => MonomorphicExpression(me.expressionType, expression))

  /** Any `Id`-machinery *references* still present in a (normalized) body — the residue the load-bearing fail-safe
    * reports (docs/effects-as-channel.md §6): a warning during U1 bring-up, a hard error from U4. A non-empty result
    * means a body shape [[normalize]] did not reach (e.g. a rare first-class combinator reference); the jvm newtype
    * representation of `Id` still keeps such residue a no-op rather than an allocation, which is why U1 only warns. Node
    * *types* are intentionally excluded — `Id[X]` legitimately survives on nodes until the U1b type/key erasure slice.
    */
  def residualIdReferences(body: MonomorphicExpression.Expression): Seq[ValueFQN] =
    collectReferences(body).filter(isIdMachinery)

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
    headFQN(node).exists(fqn =>
      fqn == WellKnownTypes.runIdFQN || fqn == WellKnownTypes.idConstructorFQN || isEffectIdMethod(fqn, "pure")
    )

  /** A two-argument `Effect[Id]` combinator whose application collapses to plain application
    * (`flatMap@Effect[Id](f, m)`/`map@Effect[Id](f, m) ⤳ f(m)`).
    */
  private def isApplyCombinatorRef(node: Sourced[MonomorphicExpression]): Boolean =
    headFQN(node).exists(fqn => isEffectIdMethod(fqn, "flatMap") || isEffectIdMethod(fqn, "map"))

  private def headFQN(node: Sourced[MonomorphicExpression]): Option[ValueFQN] =
    node.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => Some(vfqn.value)
      case _                                                        => None
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
