package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using processor: CompilationProcess): IO[Unit] = fact match
    case ArityCheckedFunction(ffqn, functionDefinition @ FunctionDefinition(_, _, NonNative(_))) =>
      processor.registerFact(TypeCheckedFunction(ffqn, functionDefinition))
    case _                                                                                       => IO.unit
}
