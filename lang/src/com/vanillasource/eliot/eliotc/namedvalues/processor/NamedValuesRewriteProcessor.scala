package com.vanillasource.eliot.eliotc.namedvalues.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.namedvalues.NamedValues
import com.vanillasource.eliot.eliotc.namedvalues.fact.{NamedValuesIndex, NamedValuesRewrittenValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionApplication,
  StringLiteral,
  ValueReference
}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The `namedValues` reflection rewrite (Option A — a syntactic residual-expression transformation, sufficient because
  * `namedValues` is only used in *runtime* position). It recognises every applied `namedValues[V](name)` in a value's
  * operator-resolved runtime body and replaces it with the fully-resolved builder chain
  * `append(append(empty[V], ref(fqn₁)), ref(fqn₂))` over the sorted [[NamedValuesIndex]] entries, carrying the
  * call's `[V]` type argument onto `append`/`empty` so the checker enforces the element-type claim by definitional
  * equality.
  *
  * Placed after `OperatorResolverProcessor` (FQNs and application structure are final) and before
  * `RecursionCheckProcessor` (whose input is repointed to this fact), so the emitted references are recursion-checked,
  * effect-checked, saturated and monomorphized exactly like hand-written code — reachability included, so the reflected
  * values are marked live with no separate root mechanism.
  *
  * A value that uses no `namedValues` reads no facts here and is wrapped unchanged, so only values that actually
  * reflect depend on the (whole-pool) enumeration.
  *
  * Fail-safes: a non-literal `name` argument, or a bare unapplied `namedValues`, is a hard error (never a silent empty
  * list); a name in the index that cannot be enumerated aborts loudly via [[getFactOrError]].
  */
class NamedValuesRewriteProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, NamedValuesRewrittenValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn, key.platform)
    ) {

  override protected def generateFromKeyAndFact(
      key: NamedValuesRewrittenValue.Key,
      value: OperatorResolvedValue
  ): CompilerIO[NamedValuesRewrittenValue] =
    value.runtime
      .traverse(body => rewrite(body.value, value.platform).map(body.as))
      .map(newRuntime => NamedValuesRewrittenValue(value.copy(runtime = newRuntime)))

  private def rewrite(
      expr: OperatorResolvedExpression,
      platform: Platform
  ): CompilerIO[OperatorResolvedExpression] =
    expr match {
      // `namedValues[V](arg)` — exactly one application over the intrinsic reference (it takes one value argument).
      case FunctionApplication(target, arg) if namedValuesTypeArgs(target.value).isDefined =>
        arg.value match {
          case StringLiteral(name) => buildChain(name, namedValuesTypeArgs(target.value).get, platform)
          case _                   =>
            compilerError(
              arg.as("The 'namedValues' reflection primitive requires a literal String name.")
            ) >> abort
        }
      // A bare, unapplied `namedValues` cannot be reflected (there is no literal name to key on).
      case ValueReference(name, _) if name.value == NamedValues.namedValuesFQN                =>
        compilerError(
          name.as("The 'namedValues' reflection primitive must be applied directly to a literal String name.")
        ) >> abort
      case other                                                                             =>
        OperatorResolvedExpression.mapChildrenM(rewrite(_, platform))(other)
    }

  /** The `[V]` type arguments of an expression when it is a reference to the `namedValues` intrinsic, else `None`. */
  private def namedValuesTypeArgs(
      expr: OperatorResolvedExpression
  ): Option[Seq[Sourced[OperatorResolvedExpression]]] =
    expr match {
      case ValueReference(name, typeArgs) if name.value == NamedValues.namedValuesFQN => Some(typeArgs)
      case _                                                                          => None
    }

  private def buildChain(
      name: Sourced[String],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]],
      platform: Platform
  ): CompilerIO[OperatorResolvedExpression] =
    getFactOrError(NamedValuesIndex.Key(name.value, platform))(
      compilerError(name.as(s"Could not enumerate values named '${name.value}' in the source pool."))
    ).map { index =>
      // The whole chain is attributed to the call's `name` position, so a downstream element-type-claim mismatch points
      // at the `namedValues(...)` call rather than at synthesized code. Built front-to-back with `append`, so the list
      // order is the index's canonical FQN order.
      val empty: OperatorResolvedExpression = ValueReference(name.as(NamedValues.listEmptyFQN), typeArgs)
      index.fqns.foldLeft(empty) { (acc, fqn) =>
        val element = ValueReference(name.as(fqn))
        val append  = ValueReference(name.as(NamedValues.listAppendFQN), typeArgs)
        FunctionApplication(name.as(FunctionApplication(name.as(append), name.as(acc))), name.as(element))
      }
    }
}
