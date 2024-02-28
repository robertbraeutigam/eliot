package com.vanillasource.eliot.eliotc.avr

import cats.Monoid
import com.vanillasource.eliot.eliotc.module.FunctionFQN

sealed trait AVRInstruction {
  def length: Int

  def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]

  def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Array[Byte]
}

object AVRInstruction {
  private case object Empty extends AVRInstruction:
    override def length: Int                                                           = 0
    override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]                     = Map.empty
    override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Array[Byte] = Array.empty

  private case class FixedBytes(b: Array[Byte]) extends AVRInstruction:
    override def length: Int                                                           = b.length
    override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]                     = Map.empty
    override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Array[Byte] = b

  def push(reg: Register): AVRInstruction         = FixedBytes(Array(1)) // TODO
  def ldi(reg: Register, b: Byte): AVRInstruction = FixedBytes(Array(2)) // TODO
  def rcall(addr: Short): AVRInstruction          = FixedBytes(Array(3)) // TODO

  given Monoid[AVRInstruction] = new Monoid[AVRInstruction] {
    override def empty: AVRInstruction = Empty

    override def combine(x: AVRInstruction, y: AVRInstruction): AVRInstruction = new AVRInstruction {
      override def length: Int = x.length + y.length

      override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int] =
        x.generateLabelsAt(pos) ++ y.generateLabelsAt(pos + x.length)

      override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Array[Byte] = {
        x.generateBytesAt(pos, labels) ++ y.generateBytesAt(pos + x.length, labels)
      }
    }
  }

  extension (instr: AVRInstruction) {
    def generateBytes(): Array[Byte] = {
      val allLabels = instr.generateLabelsAt(0)
      instr.generateBytesAt(0, allLabels)
    }
  }
}
