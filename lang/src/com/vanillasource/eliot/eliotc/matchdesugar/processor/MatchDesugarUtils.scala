package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

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

  def firstConstructorPattern(pattern: Pattern): Option[ValueFQN] =
    pattern match {
      case Pattern.ConstructorPattern(ctor, _) => Some(ctor.value)
      case _                                   => None
    }

  def collectConstructorPatterns(pattern: Pattern): Seq[ValueFQN] =
    pattern match {
      case Pattern.ConstructorPattern(ctor, subs) =>
        ctor.value +: subs.flatMap(s => collectConstructorPatterns(s.value))
      case _                                      => Seq.empty
    }

  def findAbilityMethodImpl(
      errorSource: Sourced[?],
      moduleName: ModuleName,
      abilityName: String,
      methodName: String,
      dataTypeName: Option[String] = None
  ): CompilerIO[ValueFQN] =
    for {
      moduleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      candidates   = moduleNames.names.keys.toSeq.collect {
                       case qn @ QualifiedName(n, Qualifier.AbilityImplementation(an, _))
                           if n == methodName && an.value == abilityName =>
                         qn
                     }
      selected    <- dataTypeName match {
                       case None      => candidates.headOption.pure[CompilerIO]
                       case Some(dtn) =>
                         candidates.findM(qn =>
                           ImplementationMarkerUtils
                             .firstPatternTypeConstructorName(moduleName, abilityName, qn.qualifier)
                             .map(_.contains(dtn))
                         )
                     }
      result      <- selected match
                       case Some(qn) => ValueFQN(moduleName, qn).pure[CompilerIO]
                       case None     =>
                         compilerAbort(
                           errorSource.as(s"No $abilityName.$methodName implementation found in module $moduleName.")
                         )
    } yield result
}
