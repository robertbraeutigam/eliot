package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{typeFQN, typeFQNType}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedValue, OperatorResolvedExpression as Expr}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.*
import com.vanillasource.eliot.eliotc.symbolic.fact.{QuantifiedType, SymbolicType, TypedExpression}
import com.vanillasource.eliot.eliotc.symbolic.types.{NormalFormEvaluator, SymbolicUnification}
import SymbolicType.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Symbolically evaluates and type checks an expression stack.
  *
  * These are the forms of data in this class:
  *   - OperatorResolvedExpression: These are the "raw" expressions from the previous processing step. These are
  *     organized in stacks, where every level defines the type of the next level. Also all levels may have smaller
  *     stacks as parts, like lambda bodies, parameter types, function calls, etc. It's a sort-of fractal data
  *     structure.
  *   - SymbolicType: In this class this always refers to a "normal form", i.e. a symbolic evaluation of an expression.
  *     This means it will inline and reduce all referenced functions except constructors which will stay as structural
  *     elements to unify later.
  *   - TypedExpression: A pair of a SymbolicType describing type and TypedExpression.Expression, which is the same as
  *     an OperatorResolvedExpression, except it is not stacked anymore. All type information is "flattened" to a single
  *     SymbolicType.
  */
object SymbolicTypeCheck extends Logging {

  /** Typecheck the given expression stack and return the typed expression, i.e. the result type and the expression when
    * run producing the result.
    */
  def typeCheck(expressions: NonEmptySeq[Sourced[OperatorResolvedExpression]]): TypeGraphIO[TypedExpression] =
    for {
      _      <- debug[TypeGraphIO]("Type checking top...")
      // The topmost expression needs to be of type "Type" and there is always a topmost expression (non-empty seq)
      top    <- typeCheck(TypeReference(typeFQN), expressions.head)
      // Iterate the rest, every level is the type of the next level
      result <- expressions.tail.foldLeftM((top, expressions.head)) { (acc, expression) =>
                  for {
                    // Convert previous level to normalized type expression, stripping leading
                    // TypeLambdas since their parameters are already bound in the type check state
                    previousLevel   <- StateT
                                         .liftF(NormalFormEvaluator.evaluate(acc._2))
                                         .map(st => QuantifiedType.fromSymbolicType(st).body)
                    // Create constraints of this level against the assumed type
                    _               <- debug[TypeGraphIO]("Type checking new level...")
                    typedExpression <- typeCheck(previousLevel, expression)
                  } yield (typedExpression, expression)
                }
    } yield result._1

  private def typeCheck(
      resultType: SymbolicType,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[TypedExpression] =
    debug[TypeGraphIO](
      s"Type check '${symbolicTypeUserDisplay.show(resultType)}' is type of: ${expression.value.show}"
    ) >>
      (expression.value match {
        case Expr.IntegerLiteral(value)                          =>
          // Easy, result needs to be Int
          val exprType = TypeReference(
            ValueFQN(ModuleName(Seq("eliot", "lang"), "Number"), QualifiedName("Int", CoreQualifier.Type))
          )
          tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
            TypedExpression(exprType, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]
        case Expr.StringLiteral(value)                           =>
          // Easy, result needs to be String
          val exprType = TypeReference(
            ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("String", CoreQualifier.Type))
          )
          tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
            TypedExpression(exprType, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]
        case Expr.ParameterReference(name)                       =>
          // Also easy, return parameter needs to be whatever it was declared to
          for {
            maybeType <- lookupParameter(name.value)
            exprType  <- maybeType match {
                           case Some(parameterType) => parameterType.value.pure[TypeGraphIO]
                           case None                => StateT.liftF(compilerAbort[SymbolicType](name.as(s"Parameter not found.")))
                         }
            _         <- tellConstraint(SymbolicUnification.constraint(resultType, expression.as(exprType), "Type mismatch."))
          } yield TypedExpression(exprType, TypedExpression.ParameterReference(name))
        case Expr.ValueReference(rawVfqn, typeArgs)              =>
          // This is an assumption, but Type^Default needs to be Type^Type
          val vfqn = if (rawVfqn.value === typeFQN) rawVfqn.as(typeFQNType) else rawVfqn
          // Get the return type of the value, check the type args too
          for {
            // Get the value and its signature (we don't check the whole thing, it will be checked on its own)
            resolved     <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
            rawValueType <- StateT.liftF(NormalFormEvaluator.evaluate(resolved.typeStack.map(_.signature)))
            // Instantiate quantified type params with fresh unification vars to avoid name collisions
            quantified    = QuantifiedType.fromSymbolicType(rawValueType)
            valueType    <- quantified.typeParams.foldLeftM(quantified.body) { (body, param) =>
                              generateUnificationVar.map(v => SymbolicType.substitute(body, param._1, v))
                            }
            // Constrain the result type to the valueType here
            _            <- tellConstraint(SymbolicUnification.constraint(resultType, vfqn.as(valueType), "Type mismatch."))
            // TODO: We ignore typeArgs for now, we need to check their types as well and include them somehow
            _            <-
              debug[TypeGraphIO](
                s"Inside value reference for '${vfqn.value.show}', value type: ${symbolicTypeUserDisplay.show(valueType)}"
              )
          } yield TypedExpression(valueType, TypedExpression.ValueReference(vfqn))
        case Expr.FunctionApplication(target, arg)               =>
          // In a function application we check the target, the arg and result
          for {
            argTypeVar  <- generateUnificationVar
            retTypeVar  <- generateUnificationVar
            targetTyped <- typeCheck(functionType(argTypeVar, retTypeVar), target) // TODO: Custom error message here
            argTyped    <- typeCheck(argTypeVar, arg)
            // The return type needs to be the result type
            _           <-
              tellConstraint(
                SymbolicUnification.constraint(resultType, expression.as(retTypeVar), "Type mismatch.")
              )
          } yield TypedExpression(
            retTypeVar,
            TypedExpression.FunctionApplication(target.as(targetTyped), arg.as(argTyped))
          )
        case Expr.FunctionLiteral(paramName, paramTypeOpt, body) =>
          // Check parameter type, the result type is Function[ArgType, RetType]
          for {
            typedParamType <- paramTypeOpt match {
                                case Some(paramTypeExpression) =>
                                  // Parameter type specified in the expression, so bind that
                                  for {
                                    _         <- debug[TypeGraphIO]("Checking function literal parameter type...")
                                    _         <- typeCheck(paramTypeExpression.value.levels.map(paramTypeExpression.as(_)))
                                    paramType <-
                                      StateT.liftF(
                                        NormalFormEvaluator.evaluate(paramTypeExpression.map(_.signature))
                                      )
                                  } yield paramTypeExpression.as(paramType)
                                case None                      =>
                                  // Parameter type not specified, so let's just get a unification var
                                  generateUnificationVar.map(paramName.as(_))
                              }
            _              <- bindParameter(paramName.value, typedParamType)
            _              <- addUniversalVar(paramName.value) // FIXME: not true, this is not always universal, only on top!
            _              <- debug[TypeGraphIO]("Checking function literal body type...")
            retTypeVar     <- generateUnificationVar
            typedBody      <- typeCheck(retTypeVar, body)
            _              <-
              debug[TypeGraphIO](
                s"Inside function literal, typed param type: ${symbolicTypeUserDisplay
                    .show(typedParamType.value)}, body type: ${symbolicTypeUserDisplay.show(retTypeVar)}"
              )
            funcType        = functionType(typedParamType.value, retTypeVar)
            _              <- tellConstraint(SymbolicUnification.constraint(resultType, body.as(funcType), "Type mismatch."))
          } yield TypedExpression(
            funcType,
            TypedExpression.FunctionLiteral(paramName, typedParamType, body.as(typedBody))
          )
      })
}
