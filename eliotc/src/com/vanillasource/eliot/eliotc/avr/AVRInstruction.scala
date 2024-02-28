package com.vanillasource.eliot.eliotc.avr

import cats.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.FunctionFQN

import java.nio.ByteBuffer

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

  private case class Parameterized16BitOpcode(template: String, args: (Int, Map[FunctionFQN, Int]) => Map[Char, Int])
      extends AVRInstruction:
    override def length: Int                                                           = 4
    override def generateLabelsAt(pos: Int): Map[FunctionFQN, Int]                     = Map.empty
    override def generateBytesAt(pos: Int, labels: Map[FunctionFQN, Int]): Array[Byte] = {
      val calculatedArgs = args.apply(pos, labels)
      // Check template is 16 bits
      if (template.filterNot(_ === ' ').length != 16) {
        throw new IllegalArgumentException(s"template '$template' does not appear to be a 16 bit template")
      }
      // Check all arguments are inside the template
      calculatedArgs.foreach { (name, value) =>
        val len = template.count(_ === name)
        if (len === 0) {
          throw new IllegalArgumentException(s"parameter '$name' is not in template '$template'")
        }
        if (value >= (1 << len)) {
          throw new IllegalArgumentException(
            s"parameter '$name' with value $value is greater than template '$template' has place for"
          )
        }
      }

      val value = template
        .filterNot(_ === ' ')
        .foldRight((0, calculatedArgs)) { case (nextCh, (sum, currentArgs)) =>
          nextCh match
            case '0' => (sum << 1, calculatedArgs)
            case '1' => ((sum << 1) + 1, calculatedArgs)
            case c   =>
              val argValue = currentArgs.getOrElse(
                c,
                throw new IllegalArgumentException(
                  s"template character $c in template '$template' does not refer to any existing arguments, which are: ${calculatedArgs.keys
                      .mkString(", ")}"
                )
              )
              ((sum << 1) + (argValue & 1), calculatedArgs ++ Map((c, argValue >> 1)))
        }
        ._1

      ByteBuffer
        .allocate(4)
        .order(java.nio.ByteOrder.BIG_ENDIAN)
        .putInt(value)
        .array()
    }

  def push(reg: Register): AVRInstruction            =
    Parameterized16BitOpcode("1001 001d dddd 1111", (_, _) => Map(('d', reg.ordinal)))
  def ldi(reg: Register, value: Int): AVRInstruction =
    Parameterized16BitOpcode("1110 KKKK dddd KKKK", (_, _) => Map(('d', reg.ordinal - 16), ('K', value)))
  def rcall(ffqn: FunctionFQN): AVRInstruction       =
    Parameterized16BitOpcode(
      "1101 kkkk kkkk kkkk",
      (pos, labels) =>
        Map(
          (
            'k',
            labels.getOrElse(
              ffqn,
              throw new IllegalStateException(s"label for ${ffqn.show} does not exist")
            ) - (pos + 4)
          )
        )
    )

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
