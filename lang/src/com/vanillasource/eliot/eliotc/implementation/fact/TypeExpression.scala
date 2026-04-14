package com.vanillasource.eliot.eliotc.implementation.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

/** Type-level expression used for structural matching of ability implementations. Supports free variables
  * (ParameterReference) for pattern matching with variable capture.
  */
sealed trait TypeExpression

object TypeExpression {

  case class ConcreteValue(value: GroundValue) extends TypeExpression

  case class FunctionLiteral(
      parameterName: String,
      parameterType: GroundValue,
      body: Sourced[TypeExpression]
  ) extends TypeExpression {
    override def equals(that: Any): Boolean = that match {
      case FunctionLiteral(n, t, b) => parameterName == n && parameterType == t && body.value == b.value
      case _                        => false
    }
    override def hashCode(): Int            = (parameterName, parameterType, body.value).hashCode()
  }

  case class ParameterReference(parameterName: String) extends TypeExpression

  case class FunctionApplication(
      target: Sourced[TypeExpression],
      argument: Sourced[TypeExpression]
  ) extends TypeExpression {
    override def equals(that: Any): Boolean = that match {
      case FunctionApplication(t, a) => target.value == t.value && argument.value == a.value
      case _                         => false
    }
    override def hashCode(): Int            = (target.value, argument.value).hashCode()
  }

  @tailrec
  def stripLeadingLambdas(expr: TypeExpression): TypeExpression =
    expr match {
      case FunctionLiteral(_, _, body) => stripLeadingLambdas(body.value)
      case other                       => other
    }

  def extractLeadingLambdaParams(expr: TypeExpression): Seq[(String, GroundValue)] =
    expr match {
      case FunctionLiteral(name, paramType, body) => (name, paramType) +: extractLeadingLambdaParams(body.value)
      case _                                      => Seq.empty
    }

  /** Walk a curried function type (`A -> B -> ... -> R`) and collect the argument types (`A`, `B`, ...),
    * dropping the final return type. Used to extract pattern argument types from an ability implementation's
    * marker function signature.
    */
  def extractFunctionArgTypes(expr: TypeExpression): Seq[TypeExpression] =
    expr match {
      case FunctionType(paramType, returnType) => paramType +: extractFunctionArgTypes(returnType)
      case _                                   => Seq.empty
    }

  /** Check whether two sequences of pattern arguments can be unified — i.e. whether there exists
    * a common substitution of the declared type variables that makes them structurally equal.
    *
    * This is used to detect overlapping ability implementations: if two impls of the same ability
    * have pattern argument lists that unify, then some concrete call site would match both and the
    * impls ambiguate each other.
    *
    * The caller supplies the free-variable sets for each side. The two sides' variable names are
    * assumed to potentially collide, so side 2's variables are renamed to disjoint fresh names
    * before unification.
    */
  def patternsUnify(
      args1: Seq[TypeExpression],
      vars1: Set[String],
      args2: Seq[TypeExpression],
      vars2: Set[String]
  ): Boolean = {
    if (args1.size != args2.size) return false

    // Rename vars2 to disjoint names to avoid accidental capture if both sides use e.g. "A".
    val renaming: Map[String, String] = vars2.zipWithIndex.map { case (v, i) => v -> s"$$overlap$i" }.toMap
    val allVars: Set[String]          = vars1 ++ renaming.values
    val args2r: Seq[TypeExpression]   = args2.map(renameVars(_, renaming))

    def walk(t: TypeExpression, subst: Map[String, TypeExpression]): TypeExpression = t match {
      case ParameterReference(n) if subst.contains(n) => walk(subst(n), subst)
      case _                                          => t
    }

    def occurs(name: String, t: TypeExpression, subst: Map[String, TypeExpression]): Boolean =
      walk(t, subst) match {
        case ParameterReference(n)          => n == name
        case FunctionApplication(tgt, arg)  => occurs(name, tgt.value, subst) || occurs(name, arg.value, subst)
        case _                              => false
      }

    def unify(
        t1: TypeExpression,
        t2: TypeExpression,
        subst: Map[String, TypeExpression]
    ): Option[Map[String, TypeExpression]] = {
      val w1 = walk(t1, subst)
      val w2 = walk(t2, subst)
      (w1, w2) match {
        case (ParameterReference(a), ParameterReference(b)) if a == b                => Some(subst)
        case (ParameterReference(a), _) if allVars(a)                                =>
          if (occurs(a, w2, subst)) None else Some(subst.updated(a, w2))
        case (_, ParameterReference(b)) if allVars(b)                                =>
          if (occurs(b, w1, subst)) None else Some(subst.updated(b, w1))
        case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2                      => Some(subst)
        case (FunctionApplication(tg1, ag1), FunctionApplication(tg2, ag2))          =>
          unify(tg1.value, tg2.value, subst).flatMap(s => unify(ag1.value, ag2.value, s))
        case (fa: FunctionApplication, ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)))
            if fields.size > 1 && fields.contains("$typeName") =>
          unify(fa, fromGroundValue(v, fa.target), subst)
        case (ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)), fa: FunctionApplication)
            if fields.size > 1 && fields.contains("$typeName") =>
          unify(fromGroundValue(v, fa.target), fa, subst)
        case _                                                                        => None
      }
    }

    args1.zip(args2r).foldLeft[Option[Map[String, TypeExpression]]](Some(Map.empty)) {
      case (Some(s), (a, b)) => unify(a, b, s)
      case (None, _)         => None
    }.isDefined
  }

  private def renameVars(t: TypeExpression, renaming: Map[String, String]): TypeExpression =
    t match {
      case ParameterReference(n) if renaming.contains(n) => ParameterReference(renaming(n))
      case ParameterReference(_)                         => t
      case ConcreteValue(_)                              => t
      case FunctionApplication(tgt, arg)                 =>
        FunctionApplication(tgt.map(renameVars(_, renaming)), arg.map(renameVars(_, renaming)))
      case FunctionLiteral(n, pt, body) if !renaming.contains(n) =>
        FunctionLiteral(n, pt, body.map(renameVars(_, renaming)))
      case FunctionLiteral(_, _, _)                      => t // shadowed; leave inner alone conservatively
    }

  def concreteValueOf(expr: TypeExpression): Option[GroundValue] =
    expr match {
      case ConcreteValue(value) => Some(value)
      case _                    => None
    }

  /** Convert a concrete GroundValue into application-chain form. Type structures with type arguments are decomposed
    * into FunctionApplication chains for structural matching.
    */
  def fromGroundValue(gv: GroundValue, source: Sourced[?]): TypeExpression =
    gv match {
      case GroundValue.Structure(fields, GroundValue.Type) =>
        fields.get("$typeName") match {
          case Some(typeName @ GroundValue.Direct(_: ValueFQN, _)) =>
            val typeArgFields          = fields.removed("$typeName")
            val base: TypeExpression = ConcreteValue(GroundValue.Structure(Map("$typeName" -> typeName), GroundValue.Type))
            if (typeArgFields.isEmpty) base
            else
              typeArgFields.toSeq.sortBy(_._1).foldLeft(base) { case (acc, (_, argValue)) =>
                FunctionApplication(source.as(acc), source.as(fromGroundValue(argValue, source)))
              }
          case _                                                   => ConcreteValue(gv)
        }
      case _                                               => ConcreteValue(gv)
    }

  /** Capture-avoiding substitution. */
  def substitute(body: TypeExpression, paramName: String, argValue: TypeExpression): TypeExpression =
    body match {
      case ParameterReference(name) if name == paramName                    => argValue
      case ParameterReference(_)                                            => body
      case FunctionApplication(target, arg)                                 =>
        FunctionApplication(target.map(substitute(_, paramName, argValue)), arg.map(substitute(_, paramName, argValue)))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, innerBody.map(substitute(_, paramName, argValue)))
      case _                                                                => body
    }

  /** Beta-reduce all FunctionApplication(FunctionLiteral, arg) redexes. */
  def betaReduce(expr: TypeExpression): TypeExpression =
    expr match {
      case FunctionApplication(target, arg)       =>
        betaReduce(target.value) match {
          case FunctionLiteral(name, _, body) =>
            betaReduce(substitute(body.value, name, betaReduce(arg.value)))
          case reducedTarget                  =>
            FunctionApplication(target.as(reducedTarget), arg.map(betaReduce))
        }
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, body.map(betaReduce))
      case other                                  => other
    }

  /** Match a pattern against a concrete TypeExpression, returning bindings for type variables. */
  def matchTypes(
      pattern: TypeExpression,
      concrete: TypeExpression,
      isTypeVar: String => Boolean = _ => true
  ): Map[String, TypeExpression] =
    (pattern, concrete) match {
      case (ParameterReference(name), _) if isTypeVar(name)                 =>
        Map(name -> concrete)
      case (FunctionType(p1, r1), FunctionType(p2, r2))                     =>
        matchTypes(p1, p2, isTypeVar) ++ matchTypes(r1, r2, isTypeVar)
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2))       =>
        matchTypes(t1.value, t2.value, isTypeVar) ++ matchTypes(a1.value, a2.value, isTypeVar)
      case (FunctionLiteral(_, _, patBody), FunctionLiteral(_, _, tgtBody)) =>
        matchTypes(patBody.value, tgtBody.value, isTypeVar)
      case (fa: FunctionApplication, ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)))
          if fields.size > 1 && fields.contains("$typeName") =>
        matchTypes(pattern, fromGroundValue(v, fa.target), isTypeVar)
      case (ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)), fa: FunctionApplication)
          if fields.size > 1 && fields.contains("$typeName") =>
        matchTypes(fromGroundValue(v, fa.target), concrete, isTypeVar)
      case _                                                                => Map.empty
    }

  private object FunctionType {
    def unapply(expr: TypeExpression): Option[(TypeExpression, TypeExpression)] =
      expr match {
        case FunctionApplication(target, returnType) if isFunctionApplicationOfDataType(target.value) =>
          target.value match {
            case FunctionApplication(_, paramType) => Some((paramType.value, returnType.value))
            case _                                 => None
          }
        case _                                                                                        => None
      }

    private def isFunctionApplicationOfDataType(expr: TypeExpression): Boolean =
      expr match {
        case FunctionApplication(target, _) =>
          target.value match {
            case ConcreteValue(v) => isFunctionDataType(v)
            case _                => false
          }
        case _                              => false
      }

    private def isFunctionDataType(v: GroundValue): Boolean =
      v match {
        case GroundValue.Structure(fields, _) =>
          fields.get("$typeName") match {
            case Some(GroundValue.Direct(vfqn: ValueFQN, _)) => vfqn === WellKnownTypes.functionDataTypeFQN
            case _                                            => false
          }
        case _                                => false
      }
  }

  given Show[TypeExpression] with {
    def show(expr: TypeExpression): String = expr match {
      case FunctionType(paramType, returnType)    => s"Function(${paramType.show}, ${returnType.show})"
      case ConcreteValue(v)                       => v.show
      case FunctionLiteral(name, paramType, body) => s"(($name: ${paramType.show}) -> ${body.value.show})"
      case ParameterReference(name)               => name
      case FunctionApplication(target, arg)       => s"${target.value.show}(${arg.value.show})"
    }
  }
}
