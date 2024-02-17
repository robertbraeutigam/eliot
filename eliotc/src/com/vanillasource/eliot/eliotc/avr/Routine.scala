package com.vanillasource.eliot.eliotc.avr

import com.vanillasource.eliot.eliotc.module.FunctionFQN

trait Routine {
  def generateBytes(placements: Map[FunctionFQN, Int]): Seq[Byte]
}
