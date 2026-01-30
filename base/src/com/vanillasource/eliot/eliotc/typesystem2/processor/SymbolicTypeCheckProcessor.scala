package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.*
import com.vanillasource.eliot.eliotc.typesystem2.types.*
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
          (declaredType, typedSignature, bodyType, typedBody, constraints, unificationVars) <-
            (for {
              typeResult     <- buildTypeConstraints(resolvedValue.value)
              bodyResult     <- buildBodyConstraints(body)
              _              <- tellConstraint(
                                  SymbolicUnification.constraint(
                                    typeResult.exprValue,
                                    body.as(bodyResult.exprValue),
                                    "Type mismatch."
                                  )
                                )
              constraints    <- getConstraints
              unificationVars <- getUnificationVars
            } yield (
              typeResult.exprValue,
              typeResult.typed,
              bodyResult.exprValue,
              bodyResult.typed,
              constraints,
              unificationVars
            )).runA(TypeCheckState())
          fullConstraints                                                                    = constraints |+| SymbolicUnification.unificationVars(unificationVars)
          _                                                                                 <- debug[CompilerIO](s"Constraints: ${fullConstraints.show}")
          solution                                                                          <- fullConstraints.solve()
          _                                                                                 <- debug[CompilerIO](s"Solution: ${solution.show}")
          resolvedTypedSignature                                                             = typedSignature.value.expressions.map(applySubstitutions(_, solution))
          resolvedTypedBody                                                                  = applySubstitutions(typedBody, solution)
          unifiedStack                                                                       = ExpressionStack(resolvedTypedBody +: resolvedTypedSignature, true)
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
    * param type) and returns the type as ExpressionValue and typed expression stack.
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
          result <- evaluateTypeExpression(expr, typeExpr)
        } yield TypeWithTyped.Stack(
          result.exprValue,
          typeExpr.as(ExpressionStack[TypedExpression](Seq(result.typed), typeExpr.value.hasRuntime))
        )
    }

  /** Evaluate a type expression, collecting universal variables along the way. This is analogous to the Evaluator
    * but tracks universal variables and builds TypedExpression nodes.
    */
  private def evaluateTypeExpression(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[TypeWithTyped] =
    expr match {
      // Universal variable introduction: A -> ... where A has empty type
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        for {
          _             <- addUniversalVar(paramName.value)
          _             <- tellUniversalVar(paramName.value)
          inner         <- evaluateTypeExpression(body.value.expressions.head, body)
          typedParamType = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
          typedBody      = body.as(ExpressionStack[TypedExpression](Seq(inner.typed), body.value.hasRuntime))
        } yield TypeWithTyped(
          inner.exprValue,
          TypedExpression(inner.exprValue, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody))
        )

      // Regular function literal (lambda type): (a: A) -> B becomes FunctionType
      case Expr.FunctionLiteral(paramName, paramType, body) =>
        for {
          paramResult <- buildTypeConstraints(paramType)
          _           <- bindParameter(paramName.value, paramResult.exprValue)
          bodyResult  <- buildTypeConstraints(body)
          // Represent function type using the FunctionType helper
          funcType     = functionType(paramResult.exprValue, bodyResult.exprValue)
        } yield TypeWithTyped(
          funcType,
          TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramResult.typed, bodyResult.typed))
        )

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn) =>
        isUniversalVar(vfqn.value.name).map { isUniv =>
          val exprValue =
            if (isUniv) ParameterReference(vfqn.value.name, Value.TypeType)
            else ConcreteValue(Types.dataType(vfqn.value))
          TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)))
        }

      // Parameter reference - look up bound type or keep symbolic
      case Expr.ParameterReference(name) =>
        resolveParameterRef(name)

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg) =>
        for {
          targetResult <- buildTypeConstraints(target)
          argResult    <- buildTypeConstraints(arg)
          resultType    = applyTypeApplication(targetResult.exprValue, argResult.exprValue)
        } yield TypeWithTyped(
          resultType,
          TypedExpression(resultType, TypedExpression.FunctionApplication(targetResult.typed, argResult.typed))
        )

      // Literals in types
      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]
      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.StringLiteral(value)))
          .pure[TypeGraphIO]
    }

  /** Apply a type constructor to an argument. Handles Function type specially. */
  private def applyTypeApplication(
      target: ExpressionValue,
      arg: ExpressionValue
  ): ExpressionValue =
    target match {
      // Function$DataType(A)(B) -> FunctionType(A, B)
      case FunctionApplication(ConcreteValue(v), paramType) if isFunctionType(v) =>
        functionType(paramType, arg)
      // Function$DataType(A) -> keep as partial application
      case ConcreteValue(v) if isFunctionType(v) =>
        FunctionApplication(target, arg)
      case _ =>
        FunctionApplication(target, arg)
    }

  private def isFunctionType(v: Value): Boolean =
    v match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function$DataType"
          case _ => false
        }
      case _ => false
    }

  /** Build constraints from the body expression, inferring types. */
  private def buildBodyConstraints(
      body: Sourced[Expression]
  ): TypeGraphIO[TypeWithTyped] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        val inferredType = primitiveType("Number", "Byte")
        TypeWithTyped(inferredType, TypedExpression(inferredType, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]
      case Expr.StringLiteral(value) =>
        val inferredType = primitiveType("String", "String")
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
                                   typeResult.exprValue,
                                   TypedExpression(typeResult.exprValue, TypedExpression.ValueReference(vfqn))
                                 )
                               }
                             case None           =>
                               val exprValue = ConcreteValue(Types.dataType(vfqn.value))
                               TypeWithTyped(
                                 exprValue,
                                 TypedExpression(exprValue, TypedExpression.ValueReference(vfqn))
                               ).pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar   <- generateUnificationVar(arg)
          retTypeVar   <- generateUnificationVar(body)
          targetResult <- buildBodyConstraints(target.map(_.expressions.head))
          argResult    <- buildBodyConstraints(arg.map(_.expressions.head))
          // Function type is represented using functionType helper
          expectedFuncType = functionType(argTypeVar, retTypeVar)
          _            <- tellConstraint(
                            SymbolicUnification.constraint(
                              expectedFuncType,
                              target.as(targetResult.exprValue),
                              "Target of function application is not a Function. Possibly too many arguments."
                            )
                          )
          _            <- tellConstraint(
                            SymbolicUnification.constraint(argTypeVar, arg.as(argResult.exprValue), "Argument type mismatch.")
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
          _             <- bindParameter(paramName.value, paramResult.exprValue)
          bodyResult    <- buildBodyConstraints(bodyStack.map(_.expressions.head))
          funcType       = functionType(paramResult.exprValue, bodyResult.exprValue)
          typedBodyStack =
            bodyStack.as(ExpressionStack[TypedExpression](Seq(bodyResult.typed), bodyStack.value.hasRuntime))
        } yield TypeWithTyped(
          funcType,
          TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramResult.typed, typedBodyStack))
        )
    }

  private def primitiveType(moduleName: String, typeName: String): ExpressionValue =
    ConcreteValue(Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName)))

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)
}
