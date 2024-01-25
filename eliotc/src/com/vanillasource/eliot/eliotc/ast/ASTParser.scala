package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all._
import com.vanillasource.eliot.eliotc.ast.TokenParser.{Success, NoSuccess, astParser}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.File

class ASTParser extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceTokens(file, tokens) => parseAST(file, tokens)
    case _                          => IO.unit
  }

  private def parseAST(file: File, tokens: Seq[Sourced[Token]])(using process: CompilationProcess): IO[Unit] =
    astParser.apply(TokenStream(tokens)): @unchecked match {
      case Success(ast, _)            => debug(s"generated AST: $ast") >> process.registerFact(SourceAST(file, ast))
      case NoSuccess(err, nextTokens) =>
        compilerError(file, nextTokens.first.map(_ => s"Expected $err, but encountered ${nextTokens.first.value}"))
    }
}
