package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ExpressionStack, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.*
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import com.vanillasource.eliot.eliotc.typesystem2.types.NormalizedExpression.*
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

import scala.annotation.tailrec

class SymbolicTypeCheckProcessor
    extends TransformationProcessor[ResolvedValue.Key, TypeCheckedValue.Key](key => ResolvedValue.Key(key.vfqn))
    with Logging {

  private type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.value match {
      case Some(body) =>
        for {
          (state, (typeConstraints, declaredType)) <- buildTypeConstraints(resolvedValue.typeExpression)
                                                        .run(TypeCheckState())
          _                                        <- debug[CompilerIO](s"Type constraints: ${typeConstraints.show}")
          (bodyConstraints, bodyType)              <- buildBodyConstraints(body, declaredType).runA(state)
          _                                        <- debug[CompilerIO](s"Body constraints: ${typeConstraints.show}")
          innermostBodyType                         = getInnermostReturnType(bodyType)
          allConstraints                            = typeConstraints |+| bodyConstraints |+|
                                                        SymbolicUnification.constraint(
                                                          declaredType,
                                                          body.as(innermostBodyType),
                                                          "Type mismatch."
                                                        )
          solution                                 <- allConstraints.solve()
          typedBody                                <- buildTypedExpression(body, bodyType, solution)
          typedType                                <- buildTypedStack(resolvedValue.typeExpression, solution)
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, Some(typedBody))
        )

      case None =>
        for {
          _         <- buildTypeConstraints(resolvedValue.typeExpression).runA(TypeCheckState())
          typedType <- buildTypedStack(resolvedValue.typeExpression, UnificationState())
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, None)
        )
    }

  private def noConstraints(expr: NormalizedExpression): (SymbolicUnification, NormalizedExpression) =
    (SymbolicUnification.empty, expr)

  /** Build constraints from the declared type expression. Recognizes universal variables (FunctionLiteral with empty
    * param type) and returns the normalized "inner" type.
    */
  private def buildTypeConstraints(
      typeExpr: Sourced[ExpressionStack]
  ): TypeGraphIO[(SymbolicUnification, NormalizedExpression)] =
    typeExpr.value.expressions match {
      case Seq()       =>
        generateUnificationVar[CompilerIO](typeExpr).map(noConstraints)
      case Seq(single) =>
        normalizeTypeExpression(single, typeExpr)
      case _           =>
        // Multi-level stack: bottom is runtime, next up is type
        normalizeTypeExpression(typeExpr.value.expressions(1), typeExpr)
    }

  /** Normalize a type expression, collecting universal variables along the way. */
  private def normalizeTypeExpression(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[(SymbolicUnification, NormalizedExpression)] =
    expr match {
      // Universal variable introduction: A -> ... where A has empty type
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        for {
          _                      <- addUniversalVar[CompilerIO](paramName.value)
          (innerUnif, innerNorm) <- normalizeTypeExpression(body.value.expressions.head, body)
        } yield (innerUnif |+| SymbolicUnification.universalVar(paramName.value), innerNorm)

      // Regular function literal (lambda type)
      case Expr.FunctionLiteral(paramName, paramType, body)                                        =>
        for {
          (paramUnif, paramNorm) <- buildTypeConstraints(paramType)
          _                      <- bindParameter[CompilerIO](paramName.value, paramNorm)
          (bodyUnif, bodyNorm)   <- buildTypeConstraints(body)
        } yield (paramUnif |+| bodyUnif, FunctionType(paramNorm, bodyNorm, source))

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn)                                                               =>
        StateT.inspect[CompilerIO, TypeCheckState, (SymbolicUnification, NormalizedExpression)] { state =>
          if (state.isUniversal(vfqn.value.name)) noConstraints(UniversalVar(vfqn.as(vfqn.value.name)))
          else noConstraints(ValueRef(vfqn, Seq.empty))
        }

      // Parameter reference - look up bound type or keep symbolic
      case Expr.ParameterReference(name)                                                           =>
        lookupParameter[CompilerIO](name.value).map {
          case Some(typ) => noConstraints(typ)
          case None      => noConstraints(ParameterRef(name))
        }

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg)                                                   =>
        for {
          (targetUnif, targetNorm) <- buildTypeConstraints(target)
          (argUnif, argNorm)       <- buildTypeConstraints(arg)
          result                   <- applyTypeApplication(targetNorm, argNorm, source)
        } yield (targetUnif |+| argUnif, result)

      // Literals in types
      case Expr.IntegerLiteral(value)                                                              => noConstraints(IntLiteral(value)).pure[TypeGraphIO]
      case Expr.StringLiteral(value)                                                               => noConstraints(NormalizedExpression.StringLiteral(value)).pure[TypeGraphIO]
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
    vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function"

  /** Get the innermost return type from a potentially curried FunctionType. */
  @tailrec
  private def getInnermostReturnType(expr: NormalizedExpression): NormalizedExpression =
    expr match {
      case FunctionType(_, ret, _) => getInnermostReturnType(ret)
      case other                   => other
    }

  /** Build constraints from the body expression, inferring types. */
  private def buildBodyConstraints(
      body: Sourced[Expression],
      expectedType: NormalizedExpression
  ): TypeGraphIO[(SymbolicUnification, NormalizedExpression)] =
    body.value match {
      case Expr.IntegerLiteral(value) => noConstraints(primitiveType("Number", "Byte", value)).pure[TypeGraphIO]
      case Expr.StringLiteral(value)  => noConstraints(primitiveType("String", "String", value)).pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        lookupParameter[CompilerIO](name.value).map {
          case Some(typ) => noConstraints(typ)
          case None      => noConstraints(ParameterRef(name))
        }

      case Expr.ValueReference(vfqn) =>
        for {
          maybeResolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption))
          result        <- maybeResolved match {
                             case Some(resolved) => buildTypeConstraints(resolved.typeExpression)
                             case None           => noConstraints(ValueRef(vfqn, Seq.empty)).pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar               <- generateUnificationVar[CompilerIO](arg)
          retTypeVar               <- generateUnificationVar[CompilerIO](body)
          (targetUnif, targetType) <- buildBodyConstraints(target.map(_.expressions.head), expectedType)
          (argUnif, argType)       <- buildBodyConstraints(arg.map(_.expressions.head), argTypeVar)
          targetConstraint          = SymbolicUnification.constraint(
                                        FunctionType(argTypeVar, retTypeVar, body),
                                        target.as(targetType),
                                        "Target of function application is not a Function. Possibly too many arguments."
                                      )
          argConstraint             = SymbolicUnification.constraint(argTypeVar, arg.as(argType), "Argument type mismatch.")
        } yield (targetUnif |+| argUnif |+| targetConstraint |+| argConstraint, retTypeVar)

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          (paramUnif, paramNorm) <- buildTypeConstraints(paramType)
          _                      <- bindParameter[CompilerIO](paramName.value, paramNorm)
          (bodyUnif, bodyType)   <- buildBodyConstraints(bodyStack.map(_.expressions.head), expectedType)
          bodyConstraint          =
            SymbolicUnification.constraint(expectedType, bodyStack.as(bodyType), "Lambda body type mismatch.")
        } yield (paramUnif |+| bodyUnif |+| bodyConstraint, FunctionType(paramNorm, expectedType, body))
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
      stack: Sourced[ExpressionStack],
      solution: UnificationState
  ): CompilerIO[Sourced[TypedExpressionStack]] =
    stack.value.expressions
      .traverse { expr =>
        val placeholderType = UnificationVar("stack", stack)
        buildTypedExpression(stack.as(expr), placeholderType, solution).map(_.value)
      }
      .map(exprs => stack.as(TypedExpressionStack(exprs)))
}
