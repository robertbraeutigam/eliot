package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}

/** A qualified name identifies a value inside a module. Beyond the `name` string it carries two orthogonal identity
  * dimensions: the [[Qualifier]] (a technical namespace — `Type`, `Ability`, …) and the [[Role]] (the signature split:
  * a `Runtime` twin vs. its `Signature` twin). `role` defaults to [[Role.Runtime]] and is invisible for it, so every
  * existing `QualifiedName(name, qualifier)` construction keeps producing a runtime name that renders and mangles
  * exactly as before.
  */
case class QualifiedName(name: String, qualifier: Qualifier, role: Role = Role.Runtime) {

  /** This name's `Signature`-role twin — same `name`/`qualifier`, role flipped to [[Role.Signature]]. */
  def signatureTwin: QualifiedName = copy(role = Role.Signature)
}

object QualifiedName {
  given Show[QualifiedName] with {
    override def show(name: QualifiedName): String = {
      val base =
        if (name.qualifier == Qualifier.Default) name.name
        else s"${name.name}^${Show[Qualifier].show(name.qualifier)}"
      if (name.role == Role.Runtime) base else s"$base@${Show[Role].show(name.role)}"
    }
  }

  given Eq[QualifiedName] = Eq.fromUniversalEquals
}
