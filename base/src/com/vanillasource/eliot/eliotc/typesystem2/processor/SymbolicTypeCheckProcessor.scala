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
        val body = resolvedValue.value.as(bodyExpr)
        for {
          (state, (typeConstraints, (declaredType, typedType))) <- buildTypeConstraints(resolvedValue.value).run
                                                                     .run(TypeCheckState())
          _                                                     <- debug[CompilerIO](s"Type constraints: ${typeConstraints.show}")
          ((bodyConstraints, (bodyType, bodyTyped)))            <- buildBodyConstraints(body).run.runA(state)
          _                                                     <- debug[CompilerIO](s"Body constraints: ${bodyConstraints.show}")
          topLevelConstraint                                     = SymbolicUnification.constraint(
                                                                     declaredType,
                                                                     body.as(bodyType),
                                                                     "Type mismatch."
                                                                   )
          allConstraints                                         = typeConstraints |+| bodyConstraints |+| topLevelConstraint
          solution                                              <- allConstraints.solve()
          resolvedTypedType                                      = applySubstitutionsToStack(typedType, solution)
          resolvedTypedBody                                      = body.as(applySubstitutions(bodyTyped, solution))
        } yield TypeCheckedValue(
          resolvedValue.vfqn,
          TypedValueDefinition(resolvedValue.name, resolvedTypedType, Some(resolvedTypedBody))
        )

      case None =>
        for {
          (_, (_, typedType)) <- buildTypeConstraints(resolvedValue.value).run.runA(TypeCheckState())
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
    * param type) and returns the normalized "inner" type AND the typed expression stack.
    */
  private def buildTypeConstraints(
      typeExpr: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[(NormalizedExpression, Sourced[ExpressionStack[TypedExpression]])] =
    // Use signature if available (for multi-element stacks), otherwise first expression (for wrapper stacks)
    typeExpr.value.signature.orElse(typeExpr.value.expressions.headOption) match {
      case None       =>
        for {
          uvar <- liftState(generateUnificationVar[CompilerIO](typeExpr))
        } yield (uvar, typeExpr.as(ExpressionStack[TypedExpression](Seq.empty, typeExpr.value.hasRuntime)))
      case Some(expr) =>
        for {
          (normalizedType, typedExpr) <- normalizeTypeExpression(expr, typeExpr)
        } yield (normalizedType, typeExpr.as(ExpressionStack[TypedExpression](Seq(typedExpr), typeExpr.value.hasRuntime)))
    }

  /** Normalize a type expression, collecting universal variables along the way.
    * Returns both the normalized type AND the typed expression.
    */
  private def normalizeTypeExpression(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[(NormalizedExpression, TypedExpression)] =
    expr match {
      // Universal variable introduction: A -> ... where A has empty type
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        for {
          _                           <- liftState(addUniversalVar[CompilerIO](paramName.value))
          _                           <- tellUniversalVar(paramName.value)
          (innerNorm, innerTyped)     <- normalizeTypeExpression(body.value.expressions.head, body)
          typedParamType               = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
          typedBody                    = body.as(ExpressionStack[TypedExpression](Seq(innerTyped), body.value.hasRuntime))
        } yield (innerNorm, TypedExpression(innerNorm, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody)))

      // Regular function literal (lambda type)
      case Expr.FunctionLiteral(paramName, paramType, body)                                        =>
        for {
          (paramNorm, typedParamType) <- buildTypeConstraints(paramType)
          _                           <- liftState(bindParameter[CompilerIO](paramName.value, paramNorm))
          (bodyNorm, typedBody)       <- buildTypeConstraints(body)
          funcType                     = FunctionType(paramNorm, bodyNorm, source)
        } yield (funcType, TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody)))

      // Value reference - could be a type like Int, String, or a universal var
      case Expr.ValueReference(vfqn)                                                               =>
        liftState(StateT.inspect[CompilerIO, TypeCheckState, (NormalizedExpression, TypedExpression)] { state =>
          val normalizedType = if (state.isUniversal(vfqn.value.name)) UniversalVar(vfqn.as(vfqn.value.name))
                               else ValueRef(vfqn, Seq.empty)
          (normalizedType, TypedExpression(normalizedType, TypedExpression.ValueReference(vfqn)))
        })

      // Parameter reference - look up bound type or keep symbolic
      case Expr.ParameterReference(name)                                                           =>
        liftState(lookupParameter[CompilerIO](name.value)).map {
          case Some(typ) => (typ, TypedExpression(typ, TypedExpression.ParameterReference(name)))
          case None      =>
            val paramRef = ParameterRef(name)
            (paramRef, TypedExpression(paramRef, TypedExpression.ParameterReference(name)))
        }

      // Function application in type position: A(B) means A parameterized by B
      case Expr.FunctionApplication(target, arg)                                                   =>
        for {
          (targetNorm, typedTarget) <- buildTypeConstraints(target)
          (argNorm, typedArg)       <- buildTypeConstraints(arg)
          resultType                <- applyTypeApplication(targetNorm, argNorm, source)
        } yield (resultType, TypedExpression(resultType, TypedExpression.FunctionApplication(typedTarget, typedArg)))

      // Literals in types
      case Expr.IntegerLiteral(value)                                                              =>
        val normalizedType = IntLiteral(value)
        (normalizedType, TypedExpression(normalizedType, TypedExpression.IntegerLiteral(value))).pure[TypeGraphIO]
      case Expr.StringLiteral(value)                                                               =>
        val normalizedType = NormalizedExpression.StringLiteral(value)
        (normalizedType, TypedExpression(normalizedType, TypedExpression.StringLiteral(value))).pure[TypeGraphIO]
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

  /** Build constraints from the body expression, inferring types.
    * Returns both the inferred type AND the typed expression.
    */
  private def buildBodyConstraints(
      body: Sourced[Expression]
  ): TypeGraphIO[(NormalizedExpression, TypedExpression)] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        val inferredType = primitiveType("Number", "Byte", value)
        (inferredType, TypedExpression(inferredType, TypedExpression.IntegerLiteral(value))).pure[TypeGraphIO]
      case Expr.StringLiteral(value)  =>
        val inferredType = primitiveType("String", "String", value)
        (inferredType, TypedExpression(inferredType, TypedExpression.StringLiteral(value))).pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        liftState(lookupParameter[CompilerIO](name.value)).map {
          case Some(typ) => (typ, TypedExpression(typ, TypedExpression.ParameterReference(name)))
          case None      =>
            val paramRef = ParameterRef(name)
            (paramRef, TypedExpression(paramRef, TypedExpression.ParameterReference(name)))
        }

      case Expr.ValueReference(vfqn) =>
        for {
          maybeResolved <- liftState(StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption)))
          result        <- maybeResolved match {
                             case Some(resolved) =>
                               buildTypeConstraints(resolved.value).map { case (normalizedType, _) =>
                                 (normalizedType, TypedExpression(normalizedType, TypedExpression.ValueReference(vfqn)))
                               }
                             case None           =>
                               val normalizedType = ValueRef(vfqn, Seq.empty)
                               (normalizedType, TypedExpression(normalizedType, TypedExpression.ValueReference(vfqn))).pure[TypeGraphIO]
                           }
        } yield result

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar                   <- liftState(generateUnificationVar[CompilerIO](arg))
          retTypeVar                   <- liftState(generateUnificationVar[CompilerIO](body))
          (targetType, targetTyped)    <- buildBodyConstraints(target.map(_.expressions.head))
          (argType, argTyped)          <- buildBodyConstraints(arg.map(_.expressions.head))
          _                            <- tellConstraint(
                                            SymbolicUnification.constraint(
                                              FunctionType(argTypeVar, retTypeVar, body),
                                              target.as(targetType),
                                              "Target of function application is not a Function. Possibly too many arguments."
                                            )
                                          )
          _                            <- tellConstraint(SymbolicUnification.constraint(argTypeVar, arg.as(argType), "Argument type mismatch."))
          typedTarget                   = target.as(ExpressionStack[TypedExpression](Seq(targetTyped), target.value.hasRuntime))
          typedArg                      = arg.as(ExpressionStack[TypedExpression](Seq(argTyped), arg.value.hasRuntime))
        } yield (retTypeVar, TypedExpression(retTypeVar, TypedExpression.FunctionApplication(typedTarget, typedArg)))

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        for {
          (paramNorm, typedParamType)  <- buildTypeConstraints(paramType)
          _                            <- liftState(bindParameter[CompilerIO](paramName.value, paramNorm))
          (bodyType, bodyTyped)        <- buildBodyConstraints(bodyStack.map(_.expressions.head))
          funcType                      = FunctionType(paramNorm, bodyType, body)
          typedBodyStack                = bodyStack.as(ExpressionStack[TypedExpression](Seq(bodyTyped), bodyStack.value.hasRuntime))
        } yield (funcType, TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBodyStack)))
    }

  private def primitiveType(moduleName: String, typeName: String, source: Sourced[?]): NormalizedExpression =
    ValueRef(source.as(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName)), Seq.empty)

  /** Apply substitutions to all types in a typed expression tree. */
  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    TypedExpression(
      solution.substitute(typed.expressionType),
      typed.expression match {
        case TypedExpression.IntegerLiteral(v)                  => TypedExpression.IntegerLiteral(v)
        case TypedExpression.StringLiteral(v)                   => TypedExpression.StringLiteral(v)
        case TypedExpression.ParameterReference(n)              => TypedExpression.ParameterReference(n)
        case TypedExpression.ValueReference(v)                  => TypedExpression.ValueReference(v)
        case TypedExpression.FunctionApplication(target, arg)   =>
          TypedExpression.FunctionApplication(
            applySubstitutionsToStack(target, solution),
            applySubstitutionsToStack(arg, solution)
          )
        case TypedExpression.FunctionLiteral(name, paramType, body) =>
          TypedExpression.FunctionLiteral(
            name,
            applySubstitutionsToStack(paramType, solution),
            applySubstitutionsToStack(body, solution)
          )
      }
    )

  /** Apply substitutions to all types in a typed expression stack. */
  private def applySubstitutionsToStack(
      stack: Sourced[ExpressionStack[TypedExpression]],
      solution: UnificationState
  ): Sourced[ExpressionStack[TypedExpression]] =
    stack.map(s => ExpressionStack(s.expressions.map(applySubstitutions(_, solution)), s.hasRuntime))
}
