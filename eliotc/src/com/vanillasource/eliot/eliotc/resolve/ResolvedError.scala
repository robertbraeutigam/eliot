package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN

object ResolvedError extends Logging {
  def compilerError(ffqn: FunctionFQN, message: String)(using process: CompilationProcess): IO[Unit] = for {
    resolvedFunction <- process.getFact(ResolvedFunction.Key(ffqn))
    _                <- resolvedFunction match
                          case Some(value) => ???
                          case None        =>
                            error(
                              s"could not print error '$message' for resolved function ${ffqn.show}, because resolved function is not found"
                            ) >> IO.unit
  } yield ()
}
