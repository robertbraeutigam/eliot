package com.vanillasource.eliot.eliotc.main

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.{JvmClassGenerator, JvmProgramGenerator}
import com.vanillasource.eliot.eliotc.module.processor.ModuleProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.{FunctionResolver, TypeResolver}
import com.vanillasource.eliot.eliotc.source.{
  InitSourcePaths,
  SourceContentReader,
  SourcedErrorPrinter,
  WalkSourcePaths
}
import com.vanillasource.eliot.eliotc.sugar.DesugarProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckProcessor
import com.vanillasource.eliot.eliotc.used.UsedSymbolsProcessor
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey, CompilerProcessor, Init}

case class Compiler(cmdLineArguments: CommandLineArguments) extends Logging {
  private val processors: Seq[CompilerProcessor] = Seq(
    InitSourcePaths(cmdLineArguments.paths),
    WalkSourcePaths(),
    SourceContentReader(),
    SourcedErrorPrinter(),
    Tokenizer(),
    ASTParser(),
    DesugarProcessor(),
    ModuleProcessor(),
    FunctionResolver(),
    TypeResolver(),
    TypeCheckProcessor(),
    UsedSymbolsProcessor(cmdLineArguments.mainFunction),
    JvmProgramGenerator(cmdLineArguments.mainFunction, cmdLineArguments.targetPath),
    JvmClassGenerator()
  )

  def run(): IO[Unit] = for {
    _     <- info("compiler starting...")
    engine = new CompilerEngine(processors)
    _     <- engine.resolve(Seq(Init))
  } yield ()
}
