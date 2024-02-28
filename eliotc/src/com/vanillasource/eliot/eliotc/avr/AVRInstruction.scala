package com.vanillasource.eliot.eliotc.avr

import cats.{Monad, Monoid}
import com.vanillasource.eliot.eliotc.module.FunctionFQN

sealed trait AVRInstruction {
  def length: Int

  def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]

  def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Seq[Byte]
}

object AVRInstruction {
  private case object Empty extends AVRInstruction:
    override def length: Int                                                         = 0
    override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]                   = Map.empty
    override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Seq[Byte] = Seq.empty

  private case class FixedBytes(b: Array[Byte]) extends AVRInstruction:
    override def length: Int                                                         = b.length
    override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]                   = Map.empty
    override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Seq[Byte] = b

  def push(reg: Register): AVRInstruction         = ???
  def ldi(reg: Register, b: Byte): AVRInstruction = ???
  def rcall(addr: Short): AVRInstruction          = ???

  given Monoid[AVRInstruction] = new Monoid[AVRInstruction] {
    override def empty: AVRInstruction = Empty

    override def combine(x: AVRInstruction, y: AVRInstruction): AVRInstruction = new AVRInstruction {
      override def length: Int = x.length + y.length

      override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int] =
        x.generateLabelsAt(pos) ++ y.generateLabelsAt(pos + x.length)

      override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Seq[Byte] = {
        val labels = generateLabelsAt(pos)
        generateBytesAt(pos, labels) ++ generateBytesAt(pos + x.length, labels)
      }
    }
  }
}
