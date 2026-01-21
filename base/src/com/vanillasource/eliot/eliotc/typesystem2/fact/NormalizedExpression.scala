package com.vanillasource.eliot.eliotc.typesystem2.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A normalized expression represents the symbolic normal form of an expression. This can be:
  *   - A concrete value (fully evaluated)
  *   - A symbolic term (involving parameters that cannot be evaluated)
  *   - A unification variable (to be solved)
  */
sealed trait NormalizedExpression

object NormalizedExpression {

  /** A reference to a defined value (e.g., Int, String, a data constructor). Arguments are for parameterized types like
    * List[Int].
    */
  case class ValueRef(vfqn: Sourced[ValueFQN], arguments: Seq[NormalizedExpression] = Seq.empty)
      extends NormalizedExpression

  /** A reference to a parameter (symbolic, cannot be reduced). */
  case class ParameterRef(name: Sourced[String]) extends NormalizedExpression

  /** A fully evaluated integer literal. */
  case class IntLiteral(value: Sourced[BigInt]) extends NormalizedExpression

  /** A fully evaluated string literal. */
  case class StringLiteral(value: Sourced[String]) extends NormalizedExpression

  /** A function type: Function[A, B] in normalized form. This is handled specially during unification. */
  case class FunctionType(
      paramType: NormalizedExpression,
      returnType: NormalizedExpression,
      source: Sourced[?]
  ) extends NormalizedExpression

  /** A symbolic function application that cannot be reduced. This occurs when the target or argument involves
    * parameters.
    */
  case class SymbolicApplication(
      target: NormalizedExpression,
      argument: NormalizedExpression,
      source: Sourced[?]
  ) extends NormalizedExpression

  /** A unification variable (existential, to be solved during type checking). */
  case class UnificationVar(id: String, source: Sourced[?]) extends NormalizedExpression

  /** A universal type variable (from explicit polymorphism, e.g., [A] in f[A]). */
  case class UniversalVar(name: Sourced[String]) extends NormalizedExpression

  extension (expr: NormalizedExpression) {

    /** Transform an expression by applying f to all children first, then to the result. */
    def transform(f: NormalizedExpression => NormalizedExpression): NormalizedExpression =
      f(expr match {
        case ValueRef(vfqn, args)                  => ValueRef(vfqn, args.map(_.transform(f)))
        case FunctionType(param, ret, source)      => FunctionType(param.transform(f), ret.transform(f), source)
        case SymbolicApplication(target, arg, src) => SymbolicApplication(target.transform(f), arg.transform(f), src)
        case leaf                                  => leaf
      })

    /** Check if this expression contains a variable with the given id. */
    def containsVar(varId: String): Boolean =
      expr match {
        case UnificationVar(id, _)                 => id == varId
        case ValueRef(_, args)                     => args.exists(_.containsVar(varId))
        case FunctionType(param, ret, _)           => param.containsVar(varId) || ret.containsVar(varId)
        case SymbolicApplication(target, arg, _)   => target.containsVar(varId) || arg.containsVar(varId)
        case ParameterRef(_) | IntLiteral(_) | StringLiteral(_) | UniversalVar(_) => false
      }
  }

  given Show[NormalizedExpression] = {
    case ValueRef(vfqn, args) if args.isEmpty => vfqn.value.show
    case ValueRef(vfqn, args)                 => s"${vfqn.value.show}[${args.map(_.show).mkString(", ")}]"
    case ParameterRef(name)                   => name.value
    case IntLiteral(value)                    => value.value.toString
    case StringLiteral(value)                 => s"\"${value.value}\""
    case FunctionType(param, ret, _)          => s"(${param.show}) -> ${ret.show}"
    case SymbolicApplication(target, arg, _)  => s"${target.show}(${arg.show})"
    case UnificationVar(id, _)                => s"?$id"
    case UniversalVar(name)                   => s"${name.value}"
  }
}
