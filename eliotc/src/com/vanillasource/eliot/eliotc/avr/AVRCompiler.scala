package com.vanillasource.eliot.eliotc.avr

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.avr.AVRInstruction.*
import com.vanillasource.eliot.eliotc.avr.Register.R16
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionBody, ResolvedFunction}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class AVRCompiler extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(ffqn, body) => compile(ffqn, body)
    case _                            => IO.unit

  private def compile(ffqn: FunctionFQN, body: FunctionBody)(using process: CompilationProcess): IO[Unit] = body match
    case FunctionBody.Native(keyword, args) => IO.unit // Is already native, so should exist
    case FunctionBody.NonNative(args, body) =>
      debug(s"avr compiled function ${ffqn.show}") >>
        process.registerFact(CompiledFunction(ffqn, compileNonNative(ffqn, body)))

  private def compileNonNative(functionFQN: FunctionFQN, body: Tree[Expression]): Routine = { placements =>
    val instructionsTree = body.map {
      case Expression.FunctionApplication(functionName) => Seq(Rcall(0)) // Fix relative calculation
      case Expression.IntegerLiteral(integerLiteral)    => Seq(Ldi(R16, integerLiteral.value.toByte), Push(R16))
    }

    instructionsTree.foldDepthFirstMonoid().flatMap(_.generateBytes())
  }
}
