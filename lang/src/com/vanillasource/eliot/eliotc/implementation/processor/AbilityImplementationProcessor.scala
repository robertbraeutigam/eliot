package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.fact.{AbilityImplementation, AbilityImplementationCheck}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.implementation.util.{NormalFormEvaluator, QuantifiedType, SymbolicType}

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
      deduplicated = matching.distinctBy(_.show)
      _           <- deduplicated match {
                       case Seq(implFQN) =>
                         registerFactIfClear(AbilityImplementation(abilityValueFQN, key.typeArguments, implFQN))
                       case Seq()        =>
                         handleMissingImplementation(abilityValueFQN, abilityFQN, key)
                       case multiple     =>
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
                    case Some(fqn) => fqn.pure[CompilerIO]
                    case None      =>
                      error[CompilerIO](s"Expected sibling for default '${abilityValueFQN.name.name}' but found none") >>
                        abort[ValueFQN]
                  }
      // For default implementations, point back to the ability method and pass type args.
      // Monomorphize will process the ability method's body with these type args.
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
  ): CompilerIO[Seq[ValueFQN]] =
    getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
      case None           => Seq.empty.pure[CompilerIO]
      case Some(resolved) =>
        resolved.name.value.qualifier match {
          case ResolveQualifier.AbilityImplementation(resolvedAbilityFQN, paramExprs)
              if resolvedAbilityFQN == expectedAbilityFQN =>
            for {
              symbolicParams <- resolveQualifierParams(resolved.name, paramExprs)
              signatureType  <- NormalFormEvaluator.evaluate(
                                  resolved.typeStack.as(resolved.typeStack.value.signature)
                                )
              qt              = QuantifiedType.fromSymbolicType(signatureType)
              freeVarNames    = qt.typeParams.map(_._1).toSet
              symbolicArgs    = expectedTypeArgs.map(SymbolicType.fromValue)
            } yield if (implMatchesQuery(symbolicParams, freeVarNames, symbolicArgs)) Seq(vfqn) else Seq.empty
          case _ => Seq.empty.pure[CompilerIO]
        }
    }

  private def resolveQualifierParams(
      name: Sourced[?],
      expressions: Seq[com.vanillasource.eliot.eliotc.resolve.fact.Expression]
  ): CompilerIO[Seq[SymbolicType]] =
    expressions.traverse { expression =>
      NormalFormEvaluator.evaluate(
        name.as(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(expression)))
      )
    }

  private def implMatchesQuery(
      implParams: Seq[SymbolicType],
      freeVarNames: Set[String],
      queryArgs: Seq[SymbolicType]
  ): Boolean = {
    if (implParams.size != queryArgs.size) false
    else {
      val bindings = implParams.zip(queryArgs).foldLeft(Map.empty[String, SymbolicType]) { (acc, pair) =>
        acc ++ SymbolicType.matchTypes(pair._1, pair._2, freeVarNames.contains)
      }
      implParams.zip(queryArgs).forall { (implParam, queryArg) =>
        freeVarNames.foldLeft(implParam) { case (acc, name) =>
          SymbolicType.substitute(acc, name, bindings.getOrElse(name, SymbolicType.TypeVariable(name)))
        } == queryArg
      }
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
