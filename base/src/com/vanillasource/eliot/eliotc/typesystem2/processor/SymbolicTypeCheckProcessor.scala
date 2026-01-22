package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.*
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import NormalizedExpression.*
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

class SymbolicTypeCheckProcessor
    extends TransformationProcessor[ResolvedValue.Key, TypeCheckedValue.Key](key => ResolvedValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.value.value.runtime match {
      case Some(bodyExpr) =>
        val body = resolvedValue.value.as(bodyExpr)
        for {
          (declaredType, typedSignature, bodyType, typedBody, constraints) <-
            (for {
              typeResult  <- buildTypeConstraints(resolvedValue.value)
              bodyResult  <- buildBodyConstraints(body)
              _           <- tellConstraint(
                               SymbolicUnification.constraint(
                                 typeResult.normalized,
                                 body.as(bodyResult.normalized),
                                 "Type mismatch."
                               )
                             )
              constraints <- getConstraints
            } yield (
              typeResult.normalized,
              typeResult.typed,
              bodyResult.normalized,
              bodyResult.typed,
              constraints
            )).runA(TypeCheckState())
          _                                                                <- debug[CompilerIO](s"Constraints: ${constraints.show}")
          solution                                                         <- constraints.solve()
          _                                                                <- debug[CompilerIO](s"Solution: ${solution.show}")
          resolvedTypedSignature                                            = typedSignature.value.expressions.map(applySubstitutions(_, solution))
          resolvedTypedBody                                                 = applySubstitutions(typedBody, solution)
          unifiedStack                                                      = ExpressionStack(resolvedTypedBody +: resolvedTypedSignature, true)
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          resolvedValue.name,
          resolvedValue.value.as(unifiedStack)
        )

      case None =>
        for {
          typedSignature <- buildTypeConstraints(resolvedValue.value).map(_.typed).runA(TypeCheckState())
          unifiedStack    = typedSignature.map(s => ExpressionStack(s.expressions, false))
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          resolvedValue.name,
          unifiedStack
        )
    }

  /** Build constraints from the declared type expression. Recognizes universal variables (FunctionLiteral with empty
    * param type) and returns the normalized type and typed expression stack.
    */
  private def buildTypeConstraints(
      typeExpr: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped.Stack] =
    typeExpr.value.signature.orElse(typeExpr.value.expressions.headOption) match {
      case None       =>
        for {
          uvar <- generateUnificationVar(typeExpr)
        } yield TypeWithTyped.Stack(
          uvar,
          typeExpr.as(ExpressionStack[TypedExpression](Seq.empty, typeExpr.value.hasRuntime))
        )
      case Some(expr) =>
        for {
          result <- normalizeTypeExpression(expr, typeExpr)
        } yield TypeWithTyped.Stack(
          result.normalized,
          typeExpr.as(ExpressionStack[TypedExpression](Seq(result.typed), typeExpr.value.hasRuntime))
        )
    }

  /** Normalize a type expression, collecting universal variables along the way. */
  private def normalizeTypeExpression(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[TypeWithTyped] =
    expr match {
      // Universal variable introduction: A -> ... where A has empty type
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        for {
          _             <- addUniversalVar(paramName.value)
          _             <- tellUniversalVar(paramName.value)
          inner         <- normalizeTypeExpression(body.value.expressions.head, body)
          typedParamType = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
          typedBody      = body.as(ExpressionStack[TypedExpression](Seq(inner.typed), body.value.hasRuntime))
        } yield TypeWithTyped(
          inner.normalized,
          TypedExpression(inner.normalized, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody))
        )

      // Regular function literal (lambda type)
      case Expr.FunctionLiteral(paramName, paramType, body)                                        =>
        for {
          paramResult <- buildTypeConstraints(paramType)
          _           <- bindParameter(paramName.value, paramResult.normalized)
          bodyResult  <- buildTypeConstraints(body)
          funcType     = FunctionType(paramResult.normalized, bodyResult.normalized, source)
        } yield TypeWithTyped(
          funcType,
          TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramResult.typed, bodyResult.typed))
        )

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn)                                                               =>
        isUniversalVar(vfqn.value.name).map { isUniv =>
          val normalizedType =
            if (isUniv) UniversalVar(vfqn.as(vfqn.value.name))
            else ValueRef(vfqn, Seq.empty)
          TypeWithTyped(normalizedType, TypedExpression(normalizedType, TypedExpression.ValueReference(vfqn)))
        }

      // Parameter reference - look up bound type or keep symbolic
      case Expr.ParameterReference(name)                                                           =>
        resolveParameterRef(name)

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg)                                                   =>
        for {
          targetResult <- buildTypeConstraints(target)
          argResult    <- buildTypeConstraints(arg)
          resultType   <- applyTypeApplication(targetResult.normalized, argResult.normalized, source)
        } yield TypeWithTyped(
          resultType,
          TypedExpression(resultType, TypedExpression.FunctionApplication(targetResult.typed, argResult.typed))
        )

      // Literals in types
      case Expr.IntegerLiteral(value)                                                              =>
        val normalizedType = IntLiteral(value)
        TypeWithTyped(normalizedType, TypedExpression(normalizedType, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]
      case Expr.StringLiteral(value)                                                               =>
        val normalizedType = NormalizedExpression.StringLiteral(value)
        TypeWithTyped(normalizedType, TypedExpression(normalizedType, TypedExpression.StringLiteral(value)))
          .pure[TypeGraphIO]
    }

  /** Apply a type constructor to an argument. */
  private def applyTypeApplication(
      target: NormalizedExpression,
      arg: NormalizedExpression,
      source: Sourced[?]
  ): TypeGraphIO[NormalizedExpression] =
    (target match {
      case ValueRef(vfqn, Seq()) if isFunctionType(vfqn.value)          => ValueRef(vfqn, Seq(arg))
      case ValueRef(vfqn, Seq(paramType)) if isFunctionType(vfqn.value) => FunctionType(paramType, arg, source)
      case ValueRef(vfqn, args)                                         => ValueRef(vfqn, args :+ arg)
      case _                                                            => SymbolicApplication(target, arg, source)
    }).pure[TypeGraphIO]

  private def isFunctionType(vfqn: ValueFQN): Boolean =
    vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function$DataType"

  /** Build constraints from the body expression, inferring types. */
  private def buildBodyConstraints(
      body: Sourced[Expression]
  ): TypeGraphIO[TypeWithTyped] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        val inferredType = primitiveType("Number", "Byte", value)
        TypeWithTyped(inferredType, TypedExpression(inferredType, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]
      case Expr.StringLiteral(value)  =>
        val inferredType = primitiveType("String", "String", value)
        TypeWithTyped(inferredType, TypedExpression(inferredType, TypedExpression.StringLiteral(value)))
          .pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        resolveParameterRef(name)

      case Expr.ValueReference(vfqn) =>
        for {
          maybeResolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption))
          result        <- maybeResolved match {
                             case Some(resolved) =>
                               buildTypeConstraints(resolved.value).map { typeResult =>
                                 TypeWithTyped(
                                   typeResult.normalized,
                                   TypedExpression(typeResult.normalized, TypedExpression.ValueReference(vfqn))
                                 )
                               }
                             case None           =>
                               val normalizedType = ValueRef(vfqn, Seq.empty)
                               TypeWithTyped(
                                 normalizedType,
                                 TypedExpression(normalizedType, TypedExpression.ValueReference(vfqn))
                               ).pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar   <- generateUnificationVar(arg)
          retTypeVar   <- generateUnificationVar(body)
          targetResult <- buildBodyConstraints(target.map(_.expressions.head))
          argResult    <- buildBodyConstraints(arg.map(_.expressions.head))
          _            <- tellConstraint(
                            SymbolicUnification.constraint(
                              FunctionType(argTypeVar, retTypeVar, body),
                              target.as(targetResult.normalized),
                              "Target of function application is not a Function. Possibly too many arguments."
                            )
                          )
          _            <- tellConstraint(
                            SymbolicUnification.constraint(argTypeVar, arg.as(argResult.normalized), "Argument type mismatch.")
                          )
          typedTarget   = target.as(ExpressionStack[TypedExpression](Seq(targetResult.typed), target.value.hasRuntime))
          typedArg      = arg.as(ExpressionStack[TypedExpression](Seq(argResult.typed), arg.value.hasRuntime))
        } yield TypeWithTyped(
          retTypeVar,
          TypedExpression(retTypeVar, TypedExpression.FunctionApplication(typedTarget, typedArg))
        )

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          paramResult   <- buildTypeConstraints(paramType)
          _             <- bindParameter(paramName.value, paramResult.normalized)
          bodyResult    <- buildBodyConstraints(bodyStack.map(_.expressions.head))
          funcType       = FunctionType(paramResult.normalized, bodyResult.normalized, body)
          typedBodyStack =
            bodyStack.as(ExpressionStack[TypedExpression](Seq(bodyResult.typed), bodyStack.value.hasRuntime))
        } yield TypeWithTyped(
          funcType,
          TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramResult.typed, typedBodyStack))
        )
    }

  private def primitiveType(moduleName: String, typeName: String, source: Sourced[?]): NormalizedExpression =
    ValueRef(source.as(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName)), Seq.empty)

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)
}
