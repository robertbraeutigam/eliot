package com.vanillasource.eliot.eliotc.avr

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.avr.AVRInstruction.*
import com.vanillasource.eliot.eliotc.avr.Register.R16
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionBody, FunctionDefinition, ResolvedFunction}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class AVRCompiler extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(ffqn, fdef) => compile(ffqn, fdef)
    case _                            => IO.unit

  private def compile(ffqn: FunctionFQN, fdef: FunctionDefinition)(using process: CompilationProcess): IO[Unit] =
    fdef.body match
      case FunctionBody.Native(keyword) => IO.unit // Is already native, so should exist
      case FunctionBody.NonNative(body) =>
        debug(s"avr compiled function ${ffqn.show}") >>
          process.registerFact(CompiledFunction(ffqn, compileNonNative(ffqn, body)))

  private def compileNonNative(functionFQN: FunctionFQN, body: Tree[Expression]): AVRInstruction = {
    val instructionsTree = body.map {
      case Expression.FunctionApplication(functionName) => rcall(functionName.value)
      case Expression.IntegerLiteral(integerLiteral)    => ldi(R16, integerLiteral.value.toByte) |+| push(R16)
    }

    label(functionFQN) |+| instructionsTree.foldDepthFirstMonoid() |+| ret()
  }
}
