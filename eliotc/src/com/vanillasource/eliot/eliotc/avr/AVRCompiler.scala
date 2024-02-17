package com.vanillasource.eliot.eliotc.avr

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.avr.AVRInstruction.*
import com.vanillasource.eliot.eliotc.avr.Register.R16
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionBody, ResolvedFunction}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class AVRCompiler extends CompilerProcessor {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(ffqn, body) => compile(ffqn, body)
    case _                            => IO.unit

  private def compile(ffqn: FunctionFQN, body: FunctionBody): IO[Unit] = body match
    case FunctionBody.Native(keyword, args) => IO.unit // Is already native, so should exist
    case FunctionBody.NonNative(args, body) => ???

  private def compileNonNative(functionFQN: FunctionFQN, nonNative: NonNative): Routine = { placements =>
    val instructionsTree = nonNative.body.map {
      case Expression.FunctionApplication(functionName) => Seq(Rcall(0)) // Fix relative calculation
      case Expression.IntegerLiteral(integerLiteral)    => Seq(Ldi(R16, integerLiteral.value.toByte), Push(R16))
    }

    instructionsTree.foldDepthFirstMonoid().flatMap(_.generateBytes())
  }
}
