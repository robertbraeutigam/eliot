package com.vanillasource.eliot.eliotc.namedvalues

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}

/** The fixed FQNs the `namedValues` reflection rewrite recognises and targets.
  *
  *   - [[namedValuesFQN]] is the intrinsic itself (`def namedValues[V](name: String): List[V]`), an abstract body-less
  *     signature the rewrite replaces at every applied call site, so it never needs a native implementation.
  *   - [[listEmptyFQN]] / [[listPrependFQN]] are the `List` builders the emitted chain targets
  *     (`prepend(ref₁, prepend(ref₂, … empty))`). `List` itself is a separate prerequisite (`eliot.collection`); the
  *     rewrite only names its builders, so `namedValues` can be exercised at the fact level before `List` lands.
  */
object NamedValues {
  val reflectModule: ModuleName = ModuleName(Seq("eliot", "lang"), "Reflect")

  val namedValuesFQN: ValueFQN =
    ValueFQN(reflectModule, QualifiedName("namedValues", Qualifier.Default))

  val listModule: ModuleName = ModuleName(Seq("eliot", "collection"), "List")

  val listEmptyFQN: ValueFQN =
    ValueFQN(listModule, QualifiedName("empty", Qualifier.Default))

  val listPrependFQN: ValueFQN =
    ValueFQN(listModule, QualifiedName("prepend", Qualifier.Default))
}
