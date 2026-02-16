package com.vanillasource.eliot.eliotc.indent

import cats.data.State
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.indent.LayoutGenerator.GeneratorState
import com.vanillasource.eliot.eliotc.indent.LayoutToken.ContentToken
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
        SourceLayoutTokens(
          key.uri,
          fact.tokens.as(
            head
              .map(ContentToken.apply) +: generateLayout(tail).runA(GeneratorState(head, List(1))).value
          )
        )
          .pure[CompilerIO]
      case Nil          => SourceLayoutTokens(key.uri, fact.tokens.as(Seq.empty)).pure[CompilerIO]
    }

  def generateLayout(remainingTokens: Seq[Sourced[Token]]): State[GeneratorState, Seq[Sourced[LayoutToken]]] =
    remainingTokens match {
      case head :: tail =>
        State.get[GeneratorState].flatMap { state =>
          if (head.range.from.line === state.previousToken.range.from.line) {
            // Token on same line, just transform
            generateLayout(tail).map(head.map(ContentToken.apply) +: _)
          } else {
            // Token on next line, so determine layout
            if (head.range.from.col > state.indentStack.last) {
              // Indent seems to be greater, so indent
              for {
                _               <- State.set(GeneratorState(head, state.indentStack.appended(head.range.from.col)))
                tailTransformed <- generateLayout(tail)
              } yield Seq(
                head.as(LayoutToken.Newline),
                head.as(LayoutToken.Indent),
                head.map(ContentToken.apply)
              ) ++ tailTransformed
            } else if (head.range.from.col < state.indentStack.last) {
              // Indent is less than previous, so dedent until indent level is found
              val dedents = state.indentStack.reverse.takeWhile(_ > head.range.from.col)

              for {
                _               <- State.set(
                                     GeneratorState(
                                       head,
                                       state.indentStack.take(state.indentStack.length - dedents.length)
                                     )
                                   )
                tailTransformed <- generateLayout(tail)
              } yield Seq(head.as(LayoutToken.Newline)) ++ dedents.as(head.as(LayoutToken.Dedent)) ++ Seq(
                head.map(ContentToken.apply)
              ) ++ tailTransformed
            } else {
              // Indent is the same, so just newline
              for {
                _               <- State.set(GeneratorState(head, state.indentStack))
                tailTransformed <- generateLayout(tail)
              } yield Seq(head.as(LayoutToken.Newline), head.map(ContentToken.apply)) ++ tailTransformed
            }
          }
        }
      case Nil          =>
        // Eof, so generate newline + remaining dedents
        State.get[GeneratorState].map { state =>
          state.previousToken.as(LayoutToken.Newline) +: state.indentStack.tail
            .as(state.previousToken.as(LayoutToken.Dedent))
        }
    }
}

object LayoutGenerator {
  case class GeneratorState(previousToken: Sourced[Token], indentStack: List[Position.Column])
}
