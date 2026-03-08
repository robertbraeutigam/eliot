package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression as Expr
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.{SymbolicUnification, TypeCheckState}
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.TypeGraphIO

/** Symbolically evaluates and type checks an expression stack.
  *
  * These are the forms of data in this class:
  *   - OperatorResolvedExpression: These are the "raw" expressions from the previous processing step. These are
  *     organized in stacks, where every level defines the type of the next level. Also all levels may have smaller
  *     stacks as parts, like lambda bodies, parameter types, function calls, etc. It's a sort-of fractal data
  *     structure.
  *   - ExpressionValue: In this class this always refers to a "normal form", i.e. a symbolic evaluation of an
  *     expression. This means it will inline and reduce all referenced functions except constructors which will stay as
  *     structural elements to unify later.
  *   - TypedExpression: A pair of an ExpressionValue describing type and TypedExpression.Expression, which is the same
  *     as an OperatorResolvedExpression, except it is not stacked anymore. All type information if "flattened" to a
  *     single ExpressionValue.
  */
object SymbolicEvaluator2 {

  /** Typecheck the given expression stack and return the typed expression, i.e. the result type and the expression when
    * run producing the result.
    */
  def typeCheck(expressions: NonEmptySeq[Sourced[OperatorResolvedExpression]]): TypeGraphIO[TypedExpression] =
    for {
      // The topmost expression needs to be of type "Type" and there is always a topmost expression (non-empty seq)
      top    <- typeCheck(ConcreteValue(Type), expressions.head)
      // Iterate the rest, every level is the type of the next level
      result <- expressions.tail.foldLeftM((top, expressions.head)) { (acc, expression) =>
                  for {
                    // Convert previous level to normalized type expression
                    previousLevel   <- StateT.liftF(Evaluator.toNormalFormExpressionValue(acc._2))
                    // Create constraints of this level against the assumed type
                    typedExpression <- typeCheck(previousLevel, expression)
                  } yield (typedExpression, expression)
                }
    } yield result._1

  private def typeCheck(
      resultType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[TypedExpression] =
    expression.value match {
      case Expr.IntegerLiteral(value) =>
        val exprType = ConcreteValue(
          Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), "Number"), QualifiedName("Int", CoreQualifier.Type)))
        )
        tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
          TypedExpression(exprType, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprType = ConcreteValue(
          Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("String", CoreQualifier.Type)))
        )
        tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
          TypedExpression(exprType, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]

      case Expr.ParameterReference(name) =>
        for {
          maybeType <- lookupParameter(name.value)
          exprType   = maybeType.map(_.value).getOrElse(ParameterReference(name.value, Value.Type): ExpressionValue)
          _         <- tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch."))
        } yield TypedExpression(exprType, TypedExpression.ParameterReference(name))

      case Expr.ValueReference(vfqn, typeArgs) =>
        if (vfqn.value === Types.typeFQN) {
          val exprType = ConcreteValue(Types.dataType(vfqn.value))
          tellConstraint(SymbolicUnification.constraint(resultType, vfqn.as(exprType), "Type mismatch.")) *>
            TypedExpression(exprType, TypedExpression.ValueReference(vfqn)).pure[TypeGraphIO]
        } else {
          for {
            resolved                 <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
            sourcedEvaluatedTypeArgs <- typeArgs.traverse(arg =>
                                          typeCheck(ConcreteValue(Type), arg).map(r => arg.as(r.expressionType))
                                        )
            _                        <- setExplicitTypeArgCount(sourcedEvaluatedTypeArgs.length)
            (signatureType, _)       <- SymbolicEvaluator.processStackForInstantiation(resolved.typeStack, sourcedEvaluatedTypeArgs)
            remaining                <- getExplicitTypeArgCount
            _                        <- if (remaining > 0)
                                          StateT.liftF(compilerError(vfqn.as("Too many explicit type arguments.")))
                                        else ().pure[TypeGraphIO]
            _                        <- tellConstraint(SymbolicUnification.constraint(resultType, vfqn.as(signatureType), "Type mismatch."))
          } yield TypedExpression(signatureType, TypedExpression.ValueReference(vfqn))
        }

      case Expr.FunctionApplication(target, arg) =>
        for {
          argTypeVar  <- generateUnificationVar
          retTypeVar  <- generateUnificationVar
          targetTyped <- typeCheck(target.value.levels.map(target.as(_)))
          argTyped    <- typeCheck(arg.value.levels.map(arg.as(_)))
          funcType     = functionType(argTypeVar, retTypeVar)
          _           <- tellConstraint(
                           SymbolicUnification.constraint(
                             funcType,
                             target.as(targetTyped.expressionType),
                             "Target of function application is not a Function. Possibly too many arguments."
                           )
                         )
          _           <- tellConstraint(
                           SymbolicUnification.constraint(argTypeVar, arg.as(argTyped.expressionType), "Argument type mismatch.")
                         )
          _           <- tellConstraint(
                           SymbolicUnification.constraint(resultType, expression.as(retTypeVar: ExpressionValue), "Type mismatch.")
                         )
        } yield TypedExpression(retTypeVar, TypedExpression.FunctionApplication(target.as(targetTyped), arg.as(argTyped)))

      case Expr.FunctionLiteral(paramName, paramTypeOpt, body) =>
        for {
          typedParamType <- paramTypeOpt match {
                              case Some(pt) => typeCheck(pt.value.levels.map(pt.as(_))).map(r => pt.as(r.expressionType))
                              case None     => generateUnificationVar.map(v => paramName.as(v: ExpressionValue))
                            }
          _              <- bindParameter(paramName.value, typedParamType)
          bodyTyped      <- typeCheck(body.value.levels.map(body.as(_)))
          funcType        = functionType(typedParamType.value, bodyTyped.expressionType)
          _              <- tellConstraint(SymbolicUnification.constraint(resultType, body.as(funcType), "Type mismatch."))
        } yield TypedExpression(
          funcType,
          TypedExpression.FunctionLiteral(paramName, typedParamType, body.as(bodyTyped))
        )
    }
}
