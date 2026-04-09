package com.vanillasource.eliot.eliotc.eval2.fact

import cats.Eq

/** An opaque identifier for unification metavariables. */
opaque type MetaId = Int

object MetaId {
  def apply(id: Int): MetaId = id

  extension (id: MetaId) def value: Int = id

  given Eq[MetaId] = Eq.fromUniversalEquals
}
