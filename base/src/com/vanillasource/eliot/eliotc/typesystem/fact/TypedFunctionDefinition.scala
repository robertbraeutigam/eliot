package com.vanillasource.eliot.eliotc.typesystem.fact

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.fact.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.fact.TypedFunctionDefinition.*

case class TypedFunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    body: Sourced[TypedExpression]
) extends Logging {
  def debugExpressionTypes(using CompilationProcess): IO[Unit] =
    for {
      sourceContent <- getFact(SourceContent.Key(body.file))
      _             <- debug[IO](expressionTypesDebugString(name.value, body, sourceContent.map(_.content.value).getOrElse("")))
    } yield ()
}

object TypedFunctionDefinition {
  private def expressionTypesDebugString(
      functionName: String,
      body: Sourced[TypedExpression],
      source: String
  ): String = {
    val header      = s"Function: $functionName\n"
    val expressions = collectExpressions(body)
      .map { expr =>
        val sourceText = extractSourceText(source, expr)
        val truncated  = truncateMiddle(sourceText, 60)
        val typeText   = TypeReference.unqualified.show(expr.value.expressionType)
        f"  $truncated%-60s  $typeText"
      }
      .mkString("\n")
    header + expressions
  }

  private def collectExpressions(expr: Sourced[TypedExpression]): Seq[Sourced[TypedExpression]] = {
    expr.value.expression match {
      case TypedExpression.FunctionApplication(target, argument) =>
        Seq(expr) ++ collectExpressions(target) ++ collectExpressions(argument)
      case TypedExpression.FunctionLiteral(_, body)              =>
        Seq(expr) ++ collectExpressions(body)
      case _                                                     =>
        Seq(expr)
    }
  }

  private def extractSourceText(sourceContent: String, sourced: Sourced[?]): String = {
    val lines     = sourceContent.split("\n", -1)
    val startLine = sourced.range.from.line
    val startCol  = sourced.range.from.col
    val endLine   = sourced.range.to.line
    val endCol    = sourced.range.to.col

    if (startLine < 1 || endLine < 1 || startLine > lines.length || endLine > lines.length) {
      return "<source unavailable>"
    }

    val text = if (startLine == endLine) {
      val line = lines(startLine - 1)
      if (startCol > line.length || endCol > line.length + 1) {
        "<source unavailable>"
      } else {
        line.substring(startCol - 1, Math.min(endCol - 1, line.length))
      }
    } else {
      val firstLine   = lines(startLine - 1).substring(startCol - 1)
      val middleLines = (startLine + 1 until endLine).map(i => lines(i - 1))
      val lastLine    = lines(endLine - 1).substring(0, Math.min(endCol - 1, lines(endLine - 1).length))
      (Seq(firstLine) ++ middleLines ++ Seq(lastLine)).mkString(" ")
    }

    text.replaceAll("\\s+", " ").trim
  }

  private def truncateMiddle(text: String, maxLength: Int): String = {
    if (text.length <= maxLength) {
      text
    } else {
      val sideLength = (maxLength - 3) / 2
      val leftPart   = text.take(sideLength)
      val rightPart  = text.takeRight(maxLength - sideLength - 3)
      s"$leftPart...$rightPart"
    }
  }
}
