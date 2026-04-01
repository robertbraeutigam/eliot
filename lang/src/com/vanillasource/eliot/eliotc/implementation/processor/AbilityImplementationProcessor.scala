package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.fact.{AbilityImplementation, AbilityImplementationCheck}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.util.TypeExpressionEvaluator
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class AbilityImplementationProcessor extends SingleKeyTypeProcessor[AbilityImplementation.Key] with Logging {

  override protected def generateFact(key: AbilityImplementation.Key): CompilerIO[Unit] = {
    val abilityValueFQN                   = key.abilityValueFQN
    val candidateModules: Set[ModuleName] =
      Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)

    for {
      abilityFQN  <- abilityValueFQN.name.qualifier match {
                       case Qualifier.Ability(name) => AbilityFQN(abilityValueFQN.moduleName, name).pure[CompilerIO]
                       case other                   => error[CompilerIO](s"expected Ability qualifier, got: $other") >> abort
                     }
      _           <- getFactOrAbort(AbilityImplementationCheck.Key(abilityFQN, key.typeArguments))
      candidates  <- candidateModules.toSeq.flatTraverse(module =>
                       findImplementationsInModule(module, abilityValueFQN.name.name, abilityFQN.abilityName)
                     )
      matching    <- candidates.flatTraverse(vfqn => verifyImplementation(vfqn, abilityFQN, key.typeArguments))
      deduplicated = matching.distinctBy(_._1.show)
      _           <- deduplicated match {
                       case Seq((implFQN, implTypeArgs)) =>
                         registerFactIfClear(AbilityImplementation(abilityValueFQN, key.typeArguments, implFQN, implTypeArgs))
                       case Seq()                        =>
                         handleMissingImplementation(abilityValueFQN, abilityFQN, key)
                       case multiple                     =>
                         for {
                           resolved <- getFactOrAbort(OperatorResolvedValue.Key(abilityValueFQN))
                           _        <-
                             compilerError(
                               resolved.name.as(
                                 s"Multiple ability implementations found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                                     .map(_.show)
                                     .mkString("[", ", ", "]")}."
                               )
                             )
                         } yield ()
                     }
    } yield ()
  }

  private def handleMissingImplementation(
      abilityValueFQN: ValueFQN,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Unit] =
    for {
      resolved <- getFactOrAbort(OperatorResolvedValue.Key(abilityValueFQN))
      _        <- resolved.runtime match {
                    case Some(_) =>
                      handleDefaultImplementation(resolved, abilityFQN, key)
                    case None    =>
                      compilerError(
                        resolved.name.as(
                          s"No ability implementation found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                              .map(_.show)
                              .mkString("[", ", ", "]")}."
                        )
                      )
                  }
    } yield ()

  private def handleDefaultImplementation(
      abilityResolved: OperatorResolvedValue,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Unit] = {
    val abilityValueFQN  = key.abilityValueFQN
    val candidateModules = Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)
    for {
      allImpls <- candidateModules.toSeq.flatTraverse(findAnyImplementationInModule(_, abilityFQN.abilityName))
      verified <- allImpls.flatTraverse(verifyImplementation(_, abilityFQN, key.typeArguments))
      sibling  <- verified.headOption match {
                    case Some((fqn, _)) => fqn.pure[CompilerIO]
                    case None           =>
                      error[CompilerIO](s"Expected sibling for default '${abilityValueFQN.name.name}' but found none") >>
                        abort[ValueFQN]
                  }
      _        <- registerFactIfClear(
                    AbilityImplementation(key.abilityValueFQN, key.typeArguments, abilityValueFQN, key.typeArguments)
                  )
    } yield ()
  }

  private def findAnyImplementationInModule(
      moduleName: ModuleName,
      abilityLocalName: String
  ): CompilerIO[Seq[ValueFQN]] =
    getFactOrAbort(UnifiedModuleNames.Key(moduleName)).map { names =>
      names.names.keys.toSeq.collect {
        case qn @ QualifiedName(_, Qualifier.AbilityImplementation(abilityNameSrc, _))
            if abilityNameSrc.value == abilityLocalName =>
          ValueFQN(moduleName, qn)
      }
    }

  private def findImplementationsInModule(
      moduleName: ModuleName,
      functionName: String,
      abilityLocalName: String
  ): CompilerIO[Seq[ValueFQN]] =
    getFact(UnifiedModuleNames.Key(moduleName)).map {
      case None        => Seq.empty
      case Some(names) =>
        names.names.keys.toSeq.collect {
          case qn @ QualifiedName(`functionName`, Qualifier.AbilityImplementation(abilityNameSrc, _))
              if abilityNameSrc.value == abilityLocalName =>
            ValueFQN(moduleName, qn)
        }
    }

  private def verifyImplementation(
      vfqn: ValueFQN,
      expectedAbilityFQN: AbilityFQN,
      expectedTypeArgs: Seq[Value]
  ): CompilerIO[Seq[(ValueFQN, Seq[Value])]] =
    getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
      case None           => Seq.empty.pure[CompilerIO]
      case Some(resolved) =>
        resolved.name.value.qualifier match {
          case ResolveQualifier.AbilityImplementation(resolvedAbilityFQN, paramExprs)
              if resolvedAbilityFQN == expectedAbilityFQN =>
            for {
              signatureType <- TypeExpressionEvaluator.evaluate(
                                 resolved.typeStack.as(resolved.typeStack.value.signature)
                               )
              typeParams     = ExpressionValue.extractLeadingLambdaParams(signatureType).map(_._1)
              freeVarNames   = typeParams.toSet
              evalParams    <- resolveQualifierParams(resolved.name, paramExprs, freeVarNames)
              exprArgs       = expectedTypeArgs.map(ExpressionValue.fromValue(_, resolved.name))
              matchResult    = implMatchesQueryWithBindings(evalParams, freeVarNames, exprArgs)
            } yield matchResult match {
              case Some(bindings) =>
                val implTypeArgs =
                  typeParams.map(p => bindings.get(p).flatMap(ExpressionValue.concreteValueOf).getOrElse(Value.Type))
                Seq((vfqn, implTypeArgs))
              case None           => Seq.empty
            }
          case _ => Seq.empty.pure[CompilerIO]
        }
    }

  private def resolveQualifierParams(
      name: Sourced[?],
      expressions: Seq[com.vanillasource.eliot.eliotc.resolve.fact.Expression],
      freeVarNames: Set[String]
  ): CompilerIO[Seq[ExpressionValue]] =
    expressions.traverse { expression =>
      TypeExpressionEvaluator.evaluate(
        name.as(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(expression))),
        freeVarNames = freeVarNames
      )
    }

  private def implMatchesQueryWithBindings(
      implParams: Seq[ExpressionValue],
      freeVarNames: Set[String],
      queryArgs: Seq[ExpressionValue]
  ): Option[Map[String, ExpressionValue]] = {
    if (implParams.size != queryArgs.size) None
    else {
      val bindings = implParams.zip(queryArgs).foldLeft(Map.empty[String, ExpressionValue]) { (acc, pair) =>
        acc ++ ExpressionValue.matchTypes(pair._1, pair._2, freeVarNames.contains)
      }
      val matches  = implParams.zip(queryArgs).forall { (implParam, queryArg) =>
        freeVarNames.foldLeft(implParam) { case (acc, name) =>
          ExpressionValue.substitute(acc, name, bindings.getOrElse(name, ParameterReference(name)))
        } == queryArg
      }
      if (matches) Some(bindings) else None
    }
  }

  private def collectModuleNames(v: Value): Seq[ModuleName] =
    v match {
      case Value.Structure(fields, _) =>
        fields.values.toSeq.flatMap(collectModuleNames) ++
          fields.get("$typeName").toSeq.collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn.moduleName }
      case _                          => Seq.empty
    }
}
