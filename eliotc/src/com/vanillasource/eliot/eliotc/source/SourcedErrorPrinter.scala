package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class SourcedErrorPrinter extends CompilerProcessor with Logging with User {
  override def process(fact: CompilerFact[_])(using process: CompilationProcess): IO[Unit] = fact match {
    case SourcedError(file, Sourced(PositionRange(Position(fromLine, fromCol), Position(toLine, toCol)), message)) =>
      for {
        // TODO: do this better?
        contentOption <- process.getFact[SourceContent.Key, SourceContent](SourceContent.Key(file))
        _             <- contentOption match
                           case Some(content) => compilerError(file, content.content, fromLine, fromCol, toLine, toCol, message)
                           case None          => compilerError(s"File contents for $file are not available.")
      } yield ()
    case _                                                                                                         => IO.unit
  }
}
