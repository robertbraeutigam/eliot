package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced

object MatchDesugarUtils {

  def wrapExpr(src: Sourced[?], expr: Expression): Sourced[TypeStack[Expression]] =
    src.as(TypeStack.of(expr))

  def bindingName(pattern: Pattern): Option[Sourced[String]] =
    pattern match {
      case Pattern.VariablePattern(name)   => Some(name)
      case Pattern.WildcardPattern(source) => Some(source)
      case _                               => None
    }

  def buildCurriedCall(
      scrutinee: Sourced[TypeStack[Expression]],
      ref: Expression,
      args: Seq[Sourced[TypeStack[Expression]]]
  ): Expression =
    args.foldLeft(ref) { (acc, arg) =>
      Expression.FunctionApplication(scrutinee.as(acc), arg.as(arg.value.signature))
    }

  def collectConstructorPatterns(pattern: Pattern): Seq[ValueFQN] =
    pattern match {
      case Pattern.ConstructorPattern(ctor, subs) =>
        ctor.value +: subs.flatMap(s => collectConstructorPatterns(s.value))
      case _                                      => Seq.empty
    }

  def findAbilityMethodImpl(
      moduleName: ModuleName,
      abilityName: String,
      methodName: String
  ): CompilerIO[ValueFQN] =
    for {
      moduleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
    } yield moduleNames.names.keys
      .find(qn =>
        qn.name == methodName && (qn.qualifier match {
          case Qualifier.AbilityImplementation(an, _) => an.value == abilityName
          case _                                      => false
        })
      )
      .map(qn => ValueFQN(moduleName, qn))
      .getOrElse(throw RuntimeException(s"No $abilityName $methodName implementation in module $moduleName"))
}
