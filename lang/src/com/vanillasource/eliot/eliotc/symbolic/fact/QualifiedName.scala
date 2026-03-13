package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.resolve.fact.{
  QualifiedName as ResolveQualifiedName,
  Qualifier as ResolveQualifier
}

case class QualifiedName(name: String, qualifier: Qualifier)

object QualifiedName {
  given Show[QualifiedName] with {
    override def show(name: QualifiedName): String =
      name.name + (if (name.qualifier == Qualifier.Default) ("") else ("^" + name.qualifier.toString))
  }

  given Eq[QualifiedName] = Eq.fromUniversalEquals

  def from(resolveQualifiedName: ResolveQualifiedName, qualifierParams: Seq[SymbolicType]): QualifiedName = {
    val qualifier = resolveQualifiedName.qualifier match {
      case ResolveQualifier.Default                      => Qualifier.Default
      case ResolveQualifier.Type                         => Qualifier.Type
      case ResolveQualifier.Ability(an)                  => Qualifier.Ability(an)
      case ResolveQualifier.AbilityImplementation(an, _) => Qualifier.AbilityImplementation(an, qualifierParams)
    }
    QualifiedName(resolveQualifiedName.name, qualifier)
  }

}
