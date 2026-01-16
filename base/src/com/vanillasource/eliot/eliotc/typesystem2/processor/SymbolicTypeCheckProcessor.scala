package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ExpressionStack, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.typesystem2.fact.*
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import com.vanillasource.eliot.eliotc.typesystem2.types.NormalizedExpression.*
import com.vanillasource.eliot.eliotc.typesystem2.types.SymbolicUnification.Constraint
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

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
          // Build constraints and collect universal vars from type expression
          (state, (typeConstraints, declaredType)) <- buildTypeConstraints(resolvedValue.typeExpression)
                                                        .run(TypeCheckState())
          // Build constraints from body - pass declared return type for curried functions
          (bodyConstraints, bodyType)             <-
            buildBodyConstraints(body, declaredType)
              .runA(state)
          // Get the innermost return type from the body (unwrapping curried FunctionTypes)
          innermostBodyType                        = getInnermostReturnType(bodyType)
          // Combine and solve - constrain declared return type against innermost body type
          allConstraints                          = typeConstraints |+| bodyConstraints |+|
                                                      SymbolicUnification.constraint(
                                                        declaredType,
                                                        body.as(innermostBodyType),
                                                        "Type mismatch."
                                                      )
          solution                               <- allConstraints.solve()
          // Build typed AST
          typedBody                              <- buildTypedExpression(body, bodyType, solution)
          typedType                              <- buildTypedStack(resolvedValue.typeExpression, solution)
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, Some(typedBody))
        )

      case None =>
        // No body - just validate type expression
        for {
          (_, _) <- buildTypeConstraints(resolvedValue.typeExpression).run(TypeCheckState())
          typedType <- buildTypedStack(resolvedValue.typeExpression, UnificationState())
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, typedType, None)
        )
    }

  /** Build constraints from the declared type expression. Recognizes universal variables (FunctionLiteral with empty
    * param type) and returns the normalized "inner" type.
    */
  private def buildTypeConstraints(
      typeExpr: Sourced[ExpressionStack]
  ): TypeGraphIO[(SymbolicUnification, NormalizedExpression)] =
    typeExpr.value.expressions match {
      case Seq()       =>
        // Empty stack - generate a unification var
        generateUnificationVar[CompilerIO](typeExpr).map(uvar => (Monoid[SymbolicUnification].empty, uvar))
      case Seq(single) =>
        normalizeTypeExpression(single, typeExpr)
      case _           =>
        // Multi-level stack - bottom is runtime, next up is type
        // For now, just use the second expression (index 1) as the type
        normalizeTypeExpression(typeExpr.value.expressions(1), typeExpr)
    }

  /** Normalize a type expression, collecting universal variables along the way. FunctionLiteral with empty param type
    * introduces a universal variable.
    */
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
      case Expr.FunctionLiteral(paramName, paramType, body) =>
        for {
          (paramUnif, paramNorm) <- buildTypeConstraints(paramType)
          _                      <- bindParameter[CompilerIO](paramName.value, paramNorm)
          (bodyUnif, bodyNorm)   <- buildTypeConstraints(body)
        } yield (paramUnif |+| bodyUnif, FunctionType(paramNorm, bodyNorm, source))

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn) =>
        StateT.inspect[CompilerIO, TypeCheckState, (SymbolicUnification, NormalizedExpression)] { state =>
          if (state.isUniversal(vfqn.value.name)) {
            (Monoid[SymbolicUnification].empty, UniversalVar(vfqn.as(vfqn.value.name)))
          } else {
            (Monoid[SymbolicUnification].empty, ValueRef(vfqn, Seq.empty))
          }
        }

      // Parameter reference - symbolic, stays as-is
      case Expr.ParameterReference(name) =>
        lookupParameter[CompilerIO](name.value).map {
          case Some(typ) => (Monoid[SymbolicUnification].empty, typ)
          case None      => (Monoid[SymbolicUnification].empty, ParameterRef(name))
        }

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg) =>
        for {
          (targetUnif, targetNorm) <- buildTypeConstraints(target)
          (argUnif, argNorm)       <- buildTypeConstraints(arg)
          result                   <- applyTypeApplication(targetNorm, argNorm, source)
        } yield (targetUnif |+| argUnif, result)

      // Literals in types
      case Expr.IntegerLiteral(value) =>
        (Monoid[SymbolicUnification].empty, IntLiteral(value): NormalizedExpression).pure[TypeGraphIO]
      case Expr.StringLiteral(value)  =>
        (Monoid[SymbolicUnification].empty, NormalizedExpression.StringLiteral(value): NormalizedExpression)
          .pure[TypeGraphIO]
    }

  /** Apply a type constructor to an argument. E.g., Function applied to (A, B) gives FunctionType(A, B). */
  private def applyTypeApplication(
      target: NormalizedExpression,
      arg: NormalizedExpression,
      source: Sourced[?]
  ): TypeGraphIO[NormalizedExpression] =
    target match {
      // Function type constructor - first application gives partial, second gives full FunctionType
      case ValueRef(vfqn, Seq()) if isFunctionType(vfqn.value) =>
        // Function(A) - waiting for return type
        ValueRef(vfqn, Seq(arg)).pure[TypeGraphIO]
      case ValueRef(vfqn, Seq(paramType)) if isFunctionType(vfqn.value) =>
        // Function(A)(B) = FunctionType(A, B)
        FunctionType(paramType, arg, source).pure[TypeGraphIO]
      // Other type constructors
      case ValueRef(vfqn, args) =>
        ValueRef(vfqn, args :+ arg).pure[TypeGraphIO]
      // Symbolic application
      case _ =>
        SymbolicApplication(target, arg, source).pure[TypeGraphIO]
    }

  private def isFunctionType(vfqn: ValueFQN): Boolean =
    vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function"

  /** Get the innermost return type from a potentially curried FunctionType. For example, FunctionType(A,
    * FunctionType(B, C)) returns C.
    */
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
      case Expr.IntegerLiteral(value) =>
        val litType = byteType(value)
        (Monoid[SymbolicUnification].empty, litType).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val litType = stringType(value)
        (Monoid[SymbolicUnification].empty, litType).pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        lookupParameter[CompilerIO](name.value).map {
          case Some(typ) => (Monoid[SymbolicUnification].empty, typ)
          case None      =>
            // This shouldn't happen if resolution is correct
            (Monoid[SymbolicUnification].empty, ParameterRef(name))
        }

      case Expr.ValueReference(vfqn) =>
        for {
          maybeResolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption))
          result        <- maybeResolved match {
                             case Some(resolved) =>
                               // Get the type of the referenced value and make it unique
                               buildTypeConstraints(resolved.typeExpression).map { case (unif, typ) =>
                                 // Need to freshen type variables
                                 (unif, typ)
                               }
                             case None           =>
                               // Value not found - already errored in resolution
                               (Monoid[SymbolicUnification].empty, ValueRef(vfqn, Seq.empty): NormalizedExpression)
                                 .pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar               <- generateUnificationVar[CompilerIO](arg)
          retTypeVar               <- generateUnificationVar[CompilerIO](body)
          (targetUnif, targetType) <- buildBodyConstraints(target.map(_.expressions.head), expectedType)
          (argUnif, argType)       <- buildBodyConstraints(arg.map(_.expressions.head), argTypeVar)
          // Target must be a function from argType to retType
          targetConstraint          = SymbolicUnification.constraint(
                                        FunctionType(argTypeVar, retTypeVar, body),
                                        target.as(targetType),
                                        "Target of function application is not a Function. Possibly too many arguments."
                                      )
          argConstraint             = SymbolicUnification.constraint(argTypeVar, arg.as(argType), "Argument type mismatch.")
        } yield (targetUnif |+| argUnif |+| targetConstraint |+| argConstraint, retTypeVar: NormalizedExpression)

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          (paramUnif, paramNorm) <- buildTypeConstraints(paramType)
          _                      <- bindParameter[CompilerIO](paramName.value, paramNorm)
          // The expected type for the inner body is the declared return type (expectedType)
          // The FunctionLiteral wraps it, so its body should match expectedType
          (bodyUnif, bodyType)   <- buildBodyConstraints(bodyStack.map(_.expressions.head), expectedType)
          bodyConstraint          = SymbolicUnification.constraint(expectedType, bodyStack.as(bodyType), "Lambda body type mismatch.")
        } yield (
          paramUnif |+| bodyUnif |+| bodyConstraint,
          FunctionType(paramNorm, expectedType, body): NormalizedExpression
        )
    }

  private def byteType(source: Sourced[?]): NormalizedExpression =
    ValueRef(
      source.as(ValueFQN(ModuleName(Seq("eliot", "lang"), "Number"), "Byte")),
      Seq.empty
    )

  private def stringType(source: Sourced[?]): NormalizedExpression =
    ValueRef(
      source.as(ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), "String")),
      Seq.empty
    )

  /** Build typed expression from the solution. */
  private def buildTypedExpression(
      expr: Sourced[Expression],
      exprType: NormalizedExpression,
      solution: UnificationState
  ): CompilerIO[Sourced[TypedExpression]] = {
    val resolvedType = solution.substitute(exprType)

    expr.value match {
      case Expr.IntegerLiteral(value) =>
        expr
          .as(TypedExpression(resolvedType, TypedExpression.IntegerLiteral(value)))
          .pure[CompilerIO]

      case Expr.StringLiteral(value) =>
        expr
          .as(TypedExpression(resolvedType, TypedExpression.StringLiteral(value)))
          .pure[CompilerIO]

      case Expr.ParameterReference(name) =>
        expr
          .as(TypedExpression(resolvedType, TypedExpression.ParameterReference(name)))
          .pure[CompilerIO]

      case Expr.ValueReference(vfqn) =>
        expr
          .as(TypedExpression(resolvedType, TypedExpression.ValueReference(vfqn)))
          .pure[CompilerIO]

      case Expr.FunctionApplication(target, arg) =>
        for {
          typedTarget <- buildTypedStack(target, solution)
          typedArg    <- buildTypedStack(arg, solution)
        } yield expr.as(
          TypedExpression(resolvedType, TypedExpression.FunctionApplication(typedTarget, typedArg))
        )

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          typedParamType <- buildTypedStack(paramType, solution)
          typedBody      <- buildTypedStack(bodyStack, solution)
        } yield expr.as(
          TypedExpression(resolvedType, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody))
        )
    }
  }

  /** Build typed expression stack. */
  private def buildTypedStack(
      stack: Sourced[ExpressionStack],
      solution: UnificationState
  ): CompilerIO[Sourced[TypedExpressionStack]] =
    stack.value.expressions
      .traverse { expr =>
        // For now, use a placeholder type for each expression in stack
        // A more complete implementation would track types through the stack
        val placeholderType = UnificationVar("stack", stack)
        buildTypedExpression(stack.as(expr), placeholderType, solution).map(_.value)
      }
      .map(exprs => stack.as(TypedExpressionStack(exprs)))
}
