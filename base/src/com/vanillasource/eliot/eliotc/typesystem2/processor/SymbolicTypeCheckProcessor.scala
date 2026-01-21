package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.{StateT, WriterT}
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

  private type TypeCheckStateIO[T] = StateT[CompilerIO, TypeCheckState, T]
  private type TypeGraphIO[T]      = WriterT[TypeCheckStateIO, SymbolicUnification, T]

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.value.value.runtime match {
      case Some(bodyExpr) =>
        val body         = resolvedValue.value.as(bodyExpr)
        val isBodyLambda = bodyExpr.isInstanceOf[Expr.FunctionLiteral]
        for {
          (state, (typeConstraints, declaredType)) <- buildTypeConstraints(resolvedValue.value).run
                                                        .run(TypeCheckState())
          _                                        <- debug[CompilerIO](s"Type constraints: ${typeConstraints.show}")
          (bodyConstraints, bodyType)              <- buildBodyConstraints(body, declaredType).run.runA(state)
          _                                        <- debug[CompilerIO](s"Body constraints: ${bodyConstraints.show}")
          // For lambdas, type checking is done via bodyConstraint inside buildBodyConstraints.
          // For non-lambdas, we need to compare the full declared type with the body type.
          topLevelConstraint                        = if (isBodyLambda) SymbolicUnification.empty
                                                      else SymbolicUnification.constraint(
                                                        declaredType,
                                                        body.as(bodyType),
                                                        "Type mismatch."
                                                      )
          allConstraints                            = typeConstraints |+| bodyConstraints |+| topLevelConstraint
          solution                                 <- allConstraints.solve()
          typedBody                                <- buildTypedExpression(body, bodyType, solution)
          typedType                                <- buildTypedStack(resolvedValue.value, solution)
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, Some(typedBody))
        )

      case None =>
        for {
          _         <- buildTypeConstraints(resolvedValue.value).run.runA(TypeCheckState())
          typedType <- buildTypedStack(resolvedValue.value, UnificationState())
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, None)
        )
    }

  private def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    WriterT.tell(constraint)

  private def tellUniversalVar(name: String): TypeGraphIO[Unit] =
    tellConstraint(SymbolicUnification.universalVar(name))

  private def liftState[T](stateIO: TypeCheckStateIO[T]): TypeGraphIO[T] =
    WriterT.liftF(stateIO)

  /** Build constraints from the declared type expression. Recognizes universal variables (FunctionLiteral with empty
    * param type) and returns the normalized "inner" type.
    */
  private def buildTypeConstraints(
      typeExpr: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[NormalizedExpression] =
    // Use signature if available (for multi-element stacks), otherwise first expression (for wrapper stacks)
    typeExpr.value.signature.orElse(typeExpr.value.expressions.headOption) match {
      case None       =>
        liftState(generateUnificationVar[CompilerIO](typeExpr).widen[NormalizedExpression])
      case Some(expr) =>
        normalizeTypeExpression(expr, typeExpr)
    }

  /** Normalize a type expression, collecting universal variables along the way. */
  private def normalizeTypeExpression(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[NormalizedExpression] =
    expr match {
      // Universal variable introduction: A -> ... where A has empty type
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        for {
          _         <- liftState(addUniversalVar[CompilerIO](paramName.value))
          _         <- tellUniversalVar(paramName.value)
          innerNorm <- normalizeTypeExpression(body.value.expressions.head, body)
        } yield innerNorm

      // Regular function literal (lambda type)
      case Expr.FunctionLiteral(paramName, paramType, body)                                        =>
        for {
          paramNorm <- buildTypeConstraints(paramType)
          _         <- liftState(bindParameter[CompilerIO](paramName.value, paramNorm))
          bodyNorm  <- buildTypeConstraints(body)
        } yield FunctionType(paramNorm, bodyNorm, source)

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn)                                                               =>
        liftState(StateT.inspect[CompilerIO, TypeCheckState, NormalizedExpression] { state =>
          if (state.isUniversal(vfqn.value.name)) UniversalVar(vfqn.as(vfqn.value.name))
          else ValueRef(vfqn, Seq.empty)
        })

      // Parameter reference - look up bound type or keep symbolic
      case Expr.ParameterReference(name)                                                           =>
        liftState(lookupParameter[CompilerIO](name.value)).map {
          case Some(typ) => typ
          case None      => ParameterRef(name)
        }

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg)                                                   =>
        for {
          targetNorm <- buildTypeConstraints(target)
          argNorm    <- buildTypeConstraints(arg)
          result     <- applyTypeApplication(targetNorm, argNorm, source)
        } yield result

      // Literals in types
      case Expr.IntegerLiteral(value)                                                              => IntLiteral(value).pure[TypeGraphIO]
      case Expr.StringLiteral(value)                                                               => NormalizedExpression.StringLiteral(value).pure[TypeGraphIO]
    }

  /** Apply a type constructor to an argument. For Function types, the first applied arg is the param type (A in
    * Function[A, B]), and the second is the return type (B), so we create FunctionType(paramType, returnType).
    */
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
      body: Sourced[Expression],
      expectedType: NormalizedExpression
  ): TypeGraphIO[NormalizedExpression] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        primitiveType("Number", "Byte", value).pure[TypeGraphIO]
      case Expr.StringLiteral(value)  =>
        primitiveType("String", "String", value).pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        liftState(lookupParameter[CompilerIO](name.value)).map {
          case Some(typ) => typ
          case None      => ParameterRef(name)
        }

      case Expr.ValueReference(vfqn) =>
        for {
          maybeResolved <- liftState(StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption)))
          result        <- maybeResolved match {
                             case Some(resolved) => buildTypeConstraints(resolved.value)
                             case None           => ValueRef(vfqn, Seq.empty).pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar       <- liftState(generateUnificationVar[CompilerIO](arg))
          retTypeVar       <- liftState(generateUnificationVar[CompilerIO](body))
          targetType       <- buildBodyConstraints(target.map(_.expressions.head), expectedType)
          argType          <- buildBodyConstraints(arg.map(_.expressions.head), argTypeVar)
          _                <- tellConstraint(
                                SymbolicUnification.constraint(
                                  FunctionType(argTypeVar, retTypeVar, body),
                                  target.as(targetType),
                                  "Target of function application is not a Function. Possibly too many arguments."
                                )
                              )
          _                <- tellConstraint(SymbolicUnification.constraint(argTypeVar, arg.as(argType), "Argument type mismatch."))
        } yield retTypeVar

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          paramNorm          <- buildTypeConstraints(paramType)
          _                  <- liftState(bindParameter[CompilerIO](paramName.value, paramNorm))
          expectedReturnType  = expectedType match {
                                  case FunctionType(_, ret, _) => ret
                                  case other                   => other
                                }
          bodyType           <- buildBodyConstraints(bodyStack.map(_.expressions.head), expectedReturnType)
          _                  <- tellConstraint(
                                  SymbolicUnification.constraint(expectedReturnType, bodyStack.as(bodyType), "Lambda body type mismatch.")
                                )
        } yield FunctionType(paramNorm, expectedReturnType, body)
    }

  private def primitiveType(moduleName: String, typeName: String, source: Sourced[?]): NormalizedExpression =
    ValueRef(source.as(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName)), Seq.empty)

  /** Build typed expression from the solution. */
  private def buildTypedExpression(
      expr: Sourced[Expression],
      exprType: NormalizedExpression,
      solution: UnificationState
  ): CompilerIO[Sourced[TypedExpression]] = {
    val resolvedType = solution.substitute(exprType)

    expr.value match {
      case Expr.IntegerLiteral(value)    => typed(expr, resolvedType, TypedExpression.IntegerLiteral(value))
      case Expr.StringLiteral(value)     => typed(expr, resolvedType, TypedExpression.StringLiteral(value))
      case Expr.ParameterReference(name) => typed(expr, resolvedType, TypedExpression.ParameterReference(name))
      case Expr.ValueReference(vfqn)     => typed(expr, resolvedType, TypedExpression.ValueReference(vfqn))

      case Expr.FunctionApplication(target, arg) =>
        for {
          typedTarget <- buildTypedStack(target, solution)
          typedArg    <- buildTypedStack(arg, solution)
        } yield expr.as(TypedExpression(resolvedType, TypedExpression.FunctionApplication(typedTarget, typedArg)))

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          typedParamType <- buildTypedStack(paramType, solution)
          typedBody      <- buildTypedStack(bodyStack, solution)
        } yield expr.as(
          TypedExpression(resolvedType, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody))
        )
    }
  }

  private def typed(
      expr: Sourced[Expression],
      resolvedType: NormalizedExpression,
      typedExpr: TypedExpression.Expression
  ): CompilerIO[Sourced[TypedExpression]] =
    expr.as(TypedExpression(resolvedType, typedExpr)).pure[CompilerIO]

  /** Build typed expression stack. */
  private def buildTypedStack(
      stack: Sourced[ExpressionStack[Expression]],
      solution: UnificationState
  ): CompilerIO[Sourced[ExpressionStack[TypedExpression]]] =
    stack.value.expressions
      .traverse { expr =>
        val placeholderType = UnificationVar("stack", stack)
        buildTypedExpression(stack.as(expr), placeholderType, solution).map(_.value)
      }
      .map(exprs => stack.as(ExpressionStack[TypedExpression](exprs, stack.value.hasRuntime)))
}
