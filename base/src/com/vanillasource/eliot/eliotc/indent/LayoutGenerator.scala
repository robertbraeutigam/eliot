package com.vanillasource.eliot.eliotc.indent

import cats.data.State
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.indent.LayoutGenerator.GeneratorState
import com.vanillasource.eliot.eliotc.indent.LayoutToken.*
import com.vanillasource.eliot.eliotc.pos.Position
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}

class LayoutGenerator
    extends TransformationProcessor[SourceTokens.Key, SourceLayoutTokens.Key](key => SourceTokens.Key(key.uri)) {

  override protected def generateFromKeyAndFact(
      key: SourceLayoutTokens.Key,
      fact: SourceTokens
  ): CompilerIO[SourceLayoutTokens] =
    fact.tokens.value match {
      case head :: tail =>
        val layout = head.map(ContentToken.apply) +: generateLayout(tail).runA(GeneratorState(head, List(1))).value
        SourceLayoutTokens(key.uri, fact.tokens.as(layout)).pure[CompilerIO]
      case Nil          =>
        SourceLayoutTokens(key.uri, fact.tokens.as(Seq.empty)).pure[CompilerIO]
    }

  private def generateLayout(tokens: Seq[Sourced[Token]]): State[GeneratorState, Seq[Sourced[LayoutToken]]] =
    tokens match {
      case head :: tail =>
        for {
          prefix <- layoutPrefix(head)
          rest   <- generateLayout(tail)
        } yield (prefix :+ head.map(ContentToken.apply)) ++ rest
      case Nil          =>
        State.get[GeneratorState].map { state =>
          state.previousToken.as(Newline) +: state.indentStack.tail.as(state.previousToken.as(Dedent))
        }
    }

  private def layoutPrefix(token: Sourced[Token]): State[GeneratorState, Seq[Sourced[LayoutToken]]] =
    State.get[GeneratorState].flatMap { state =>
      if (token.range.from.line === state.previousToken.range.from.line) {
        State.pure(Seq.empty)
      } else {
        val col     = token.range.from.col
        val current = state.indentStack.last
        val (newStack, layoutTokens) =
          if (col > current) {
            (state.indentStack.appended(col), Seq(Newline, Indent))
          } else if (col < current) {
            val dedentCount = state.indentStack.reverse.takeWhile(_ > col).length
            (state.indentStack.dropRight(dedentCount), Newline +: List.fill(dedentCount)(Dedent))
          } else {
            (state.indentStack, Seq(Newline))
          }
        State.set(GeneratorState(token, newStack)).as(layoutTokens.map(token.as(_)))
      }
    }
}

object LayoutGenerator {
  case class GeneratorState(previousToken: Sourced[Token], indentStack: List[Position.Column])
}
