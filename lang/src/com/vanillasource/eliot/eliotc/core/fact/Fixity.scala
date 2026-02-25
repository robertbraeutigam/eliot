package com.vanillasource.eliot.eliotc.core.fact

sealed trait Fixity

object Fixity {
  case object Application                               extends Fixity
  case object Prefix                                    extends Fixity
  case class Infix(associativity: Fixity.Associativity) extends Fixity
  case object Postfix                                   extends Fixity

  sealed trait Associativity

  object Associativity {
    case object Left  extends Associativity
    case object Right extends Associativity
    case object None  extends Associativity
  }
}
