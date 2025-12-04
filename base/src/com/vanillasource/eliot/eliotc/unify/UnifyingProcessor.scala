package com.vanillasource.eliot.eliotc.unify

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class UnifyingProcessor extends OneToOneProcessor((key: UnifiedSourceAST.Key) => SourceAST.Key(key.path)) with Logging {

  override def generateFromFact(sourceAst: SourceAST)(using process: CompilationProcess): IO[Unit] =
    process.registerFact(UnifiedSourceAST(sourceAst.path, sourceAst.asts.head)) // TODO
}
