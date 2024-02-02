package com.vanillasource.eliot.eliotc.module

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ast.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.File

class ModuleFunctionsExtractor extends CompilerProcessor {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(file, ast) => extractFunctionNames(file, ast)
    case _                    => IO.unit
  }

  private def extractFunctionNames(file: File, ast: AST): IO[Unit] = ???
}
