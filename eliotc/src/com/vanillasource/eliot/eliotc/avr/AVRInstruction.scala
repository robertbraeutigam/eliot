package com.vanillasource.eliot.eliotc.avr

sealed trait AVRInstruction {
  def generateBytes(): Seq[Byte] = Seq(1, 2)
}

object AVRInstruction {
  case class Push(reg: Register)           extends AVRInstruction
  case class Ldi(reg: Register, b: Byte)   extends AVRInstruction
  case class Rcall(relativeAddress: Short) extends AVRInstruction
}
