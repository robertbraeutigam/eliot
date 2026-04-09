package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{
  bigIntType,
  fullyQualifiedNameType,
  functionDataTypeFQN,
  stringType
}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.fact.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.ConstraintExtract.collectConstraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.{
  Constraints,
  ShortUniqueIdentifiers,
  TypeCheckState
}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.ConstraintSolver.solve
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.Solution
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, MonomorphicValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    for {
      _                    <-
        debug[CompilerIO](
          s"Type checking ${key.vfqn.show}, with type arguments: ${key.specifiedTypeArguments.map(_.show).mkString(", ")}"
        )
      endState             <- collectConstraints(key, resolvedValue).runS(TypeCheckState())
      _                    <- Constraints.debugConstraints(endState.constraints)
      solution             <- solve(endState.constraints, endState.shortIds)
      _                    <- debug[CompilerIO](s"Solution (of ${key.vfqn.show}): ${solution.show}")
      (signature, runtime) <- typeSubstitute(key, solution, endState, resolvedValue)
    } yield MonomorphicValue(
      key.vfqn,
      key.specifiedTypeArguments,
      Seq.empty, // TODO: left out for now
      signature,
      runtime
    )

  // --- Phase 3: Type substitution and MonomorphicExpression building ---

  private def typeSubstitute(
      key: MonomorphicValue.Key,
      solution: Solution,
      endState: TypeCheckState,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(Value, Option[Sourced[MonomorphicExpression.Expression]])] =
    for {
      typeArgValues <- key.specifiedTypeArguments.toList.traverse { arg =>
                         Evaluator.evaluate(arg).flatMap { ev =>
                           ExpressionValue.concreteValueOf(ev) match {
                             case Some(v) => v.pure[CompilerIO]
                             case None    =>
                               compilerAbort(arg.as("Type argument did not evaluate to concrete value."))
                           }
                         }
                       }
      typeExprValue <- Evaluator.evaluate(resolvedValue.typeStack.map(_.signature))
      signature     <- Evaluator.applyTypeArgs(typeExprValue, typeArgValues, resolvedValue.name)
      paramTypes    <- resolveParameterTypes(endState, solution)
      runtime       <- resolvedValue.runtime.traverse { body =>
                         walkBody(body, signature, paramTypes, solution, resolvedValue).map(body.as)
                       }
    } yield (signature, runtime.map(_.map(_.expression)))

  private def resolveParameterTypes(
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[Map[String, Value]] =
    endState.parameterTypes.toList
      .traverse { case (name, sourced) =>
        Evaluator.evaluate(sourced).map { ev =>
          ExpressionValue.concreteValueOf(solution.resolveExpressionValue(ev)).map(name -> _)
        }
      }
      .map(_.flatten.toMap)

  // --- Replayed walk over the body ---
  //
  // The constraint extractor's traversal is deterministic in its use of fresh unification
  // variables: at every FA it generates argTypeVar then retTypeVar (before recursing), and at
  // every FL it generates a paramVar (only when the param is unannotated) then retTypeVar.
  // The Solution map is keyed by these names.
  //
  // This walk replays exactly the same generateNext sequence, threaded through a state monad
  // over ShortUniqueIdentifiers, so the names line up. To start at the right index, we first
  // walk the type-stack levels in the same reversed order with `advanceForExpression` (which
  // generates fresh vars at the same positions but discards the results), advancing the state
  // past the type-level vars before the body walk begins.
  //
  // The shortcut that avoids needing to look up retTypeVar at every FA / FL: the extractor
  // emits `assumedType := retTypeVar` at the end of FA processing, so after solving they are
  // equivalent. The processor only needs to look up `argTypeVar` and can use the parent-
  // provided assumed type directly for the result type. Similarly for FL: the body's assumed
  // type is `assumedType.asFunctionType._2`.

  private type WalkIO[A] = StateT[CompilerIO, ShortUniqueIdentifiers, A]

  private def generateNext: WalkIO[String] =
    StateT { state =>
      val (id, newState) = state.generateNext()
      (newState, id).pure[CompilerIO]
    }

  private def liftCompilerIO[A](io: CompilerIO[A]): WalkIO[A] = StateT.liftF(io)

  private def walkBody(
      body: Sourced[OperatorResolvedExpression],
      signature: Value,
      paramTypes: Map[String, Value],
      solution: Solution,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicExpression] = {
    val typeLevels = resolvedValue.typeStack.value.levels.reverse.toList
    val walk       = for {
      _    <- typeLevels.traverse_(advanceForExpression)
      expr <- buildExpression(body.value, paramTypes, body, signature, solution)
    } yield expr
    walk.runA(ShortUniqueIdentifiers())
  }

  /** Walks an expression purely to advance the ShortUniqueIdentifiers state, mirroring the
    * extractor's walk order without producing any output. Used for type-stack levels which
    * the processor doesn't need to materialize but whose fresh-var generation must be replayed.
    */
  private def advanceForExpression(expr: OperatorResolvedExpression): WalkIO[Unit] = expr match {
    case OperatorResolvedExpression.FunctionApplication(target, arg)         =>
      for {
        _ <- generateNext
        _ <- generateNext
        _ <- advanceForExpression(target.value)
        _ <- advanceForExpression(arg.value)
      } yield ()
    case OperatorResolvedExpression.FunctionLiteral(_, paramTypeOpt, body)   =>
      for {
        _ <- paramTypeOpt match {
               case Some(_) => ().pure[WalkIO]
               case None    => generateNext.void
             }
        _ <- generateNext
        _ <- advanceForExpression(body.value)
      } yield ()
    case _: OperatorResolvedExpression.ValueReference
        | _: OperatorResolvedExpression.IntegerLiteral
        | _: OperatorResolvedExpression.StringLiteral
        | _: OperatorResolvedExpression.ParameterReference                   =>
      ().pure[WalkIO]
  }

  private def buildExpression(
      expression: OperatorResolvedExpression,
      paramTypes: Map[String, Value],
      source: Sourced[?],
      assumedType: Value,
      solution: Solution
  ): WalkIO[MonomorphicExpression] = expression match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      MonomorphicExpression(bigIntType, MonomorphicExpression.IntegerLiteral(value)).pure[WalkIO]

    case OperatorResolvedExpression.StringLiteral(value) =>
      MonomorphicExpression(stringType, MonomorphicExpression.StringLiteral(value)).pure[WalkIO]

    case OperatorResolvedExpression.ParameterReference(name) =>
      val paramType = paramTypes.getOrElse(name.value, assumedType)
      MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(name)).pure[WalkIO]

    case OperatorResolvedExpression.ValueReference(vfqn, _) =>
      // The assumed type passed in by the parent IS the inferred concrete type at this call
      // site. The constraint extractor emitted `assumedType := id's polytype signature`, the
      // solver instantiated the polytype and propagated the bindings, and the parent FA in
      // this walk computed `targetType = Function(argType, assumedType)` from the resolved
      // argType. We trust that.
      MonomorphicExpression(
        assumedType,
        MonomorphicExpression.MonomorphicValueReference(vfqn, Seq.empty)
      ).pure[WalkIO]

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        argTypeVarName <- generateNext // matches extractor's argTypeVar
        _              <- generateNext // matches extractor's retTypeVar (equivalent to assumedType post-solve)
        argType        <- liftCompilerIO(lookupSolutionVar(argTypeVarName, solution, source))
        targetType      = functionTypeValue(argType, assumedType)
        targetExpr     <- buildExpression(target.value, paramTypes, target, targetType, solution)
        argExpr        <- buildExpression(arg.value, paramTypes, arg, argType, solution)
      } yield MonomorphicExpression(
        assumedType,
        MonomorphicExpression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
      )

    case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
      for {
        _         <- paramTypeOpt match {
                       case Some(_) => ().pure[WalkIO]
                       case None    => generateNext.void
                     }
        _         <- generateNext // matches extractor's retTypeVar
        paramType  = paramTypes.getOrElse(paramName.value, Value.Type)
        bodyType   = assumedType.asFunctionType.map(_._2).getOrElse(assumedType)
        bodyExpr  <- buildExpression(body.value, paramTypes, body, bodyType, solution)
      } yield MonomorphicExpression(
        assumedType,
        MonomorphicExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
      )
  }

  private def functionTypeValue(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map(
        "$typeName" -> Value.Direct(functionDataTypeFQN, fullyQualifiedNameType),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  private def lookupSolutionVar(
      name: String,
      solution: Solution,
      source: Sourced[?]
  ): CompilerIO[Value] =
    solution.substitutions.get(name) match {
      case Some(v) => v.pure[CompilerIO]
      case None    =>
        compilerAbort(source.as(s"Unification variable not resolved: $name"))
    }

}
