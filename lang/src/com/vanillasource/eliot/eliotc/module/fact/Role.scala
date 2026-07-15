package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}

/** The role dimension on a value's identity (the signature split): every named value splits at birth into a `Runtime`
  * twin — its term/alias body — and a `Signature` twin — its type/kind body. The role is **orthogonal** to
  * [[Qualifier]]: every qualifier (`Default`, `Type`, `Ability`, `AbilityImplementation`) carries both roles.
  *
  * `Runtime` is the default and is **invisible** — a runtime twin's [[QualifiedName]] renders (via `Show`) and mangles
  * byte-identically to a pre-split name, so diagnostics, JVM class names, and test expectations do not churn. A
  * `Signature` twin is a compile-time-only value (types are evaluated by the compiler), never reaching codegen.
  */
enum Role {
  case Runtime, Signature
}

object Role {
  given Show[Role] = {
    case Runtime   => "Runtime"
    case Signature => "Signature"
  }

  given Eq[Role] = Eq.fromUniversalEquals
}
