package com.vanillasource.eliot.eliotc.namedvalues

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}

/** The fixed FQNs the reflection rewrite recognises and targets.
  *
  *   - [[foldNamedValuesFQN]] is the general primitive (`def foldNamedValues[B, C](name: String, initial: B, combine:
  *     C): B`), an abstract body-less signature the rewrite replaces at every applied call site, so it never needs a
  *     native implementation. It reifies the enumeration as *code* — a right fold, `combine(fqn₁, ref₁, combine(fqn₂,
  *     ref₂, initial))` — rather than as data.
  *   - [[namedValuesFQN]] is the `List` sugar over it (`def namedValues[V](name: String): List[V]`), which is exactly
  *     that fold at the free monoid: [[listPrependFQN]] over [[listEmptyFQN]]. Right-folding with `prepend` keeps the
  *     index's canonical FQN order. `List` is represented on the JVM as a native `java.util.List` (immutable by
  *     contract).
  */
object NamedValues {
  val reflectModule: ModuleName = ModuleName(ModuleName.compilerPackage, "Reflect")

  val namedValuesFQN: ValueFQN =
    ValueFQN(reflectModule, QualifiedName("namedValues", Qualifier.Default))

  val foldNamedValuesFQN: ValueFQN =
    ValueFQN(reflectModule, QualifiedName("foldNamedValues", Qualifier.Default))

  val listModule: ModuleName = ModuleName(Seq("eliot", "collection"), "List")

  val listEmptyFQN: ValueFQN =
    ValueFQN(listModule, QualifiedName("empty", Qualifier.Default))

  val listPrependFQN: ValueFQN =
    ValueFQN(listModule, QualifiedName("prepend", Qualifier.Default))
}
