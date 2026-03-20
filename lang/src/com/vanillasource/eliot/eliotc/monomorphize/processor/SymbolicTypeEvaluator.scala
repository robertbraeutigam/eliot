package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Types, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{QuantifiedType, SymbolicType}
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*

/** Evaluates SymbolicType types to Value. Type parameters are substituted directly in SymbolicType,
  * then the ground type is materialized to a Value by resolving data type constructors via NamedEvaluable.
  */
object SymbolicTypeEvaluator extends Logging {

  /** Evaluate a SymbolicType signature (with leading TypeLambdas for type params) with concrete type arguments to a
    * Value. The leading TypeLambdas are stripped, their parameters substituted with the given type arguments, and the
    * result is materialized to a Value.
    */
  def evaluate(
      signature: SymbolicType,
      typeArgs: Seq[Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    val qt           = QuantifiedType.fromSymbolicType(signature)
    val substitution = qt.typeParams.map(_._1).zip(typeArgs).toMap
    materializeToValue(qt.body, substitution, source)
  }

  /** Evaluate a SymbolicType to a Value with a substitution map. Used for evaluating type expressions within function
    * bodies where type parameters need to be substituted with their concrete values.
    */
  def evaluateWithSubstitution(
      st: SymbolicType,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    materializeToValue(st, substitution, source)

  private def materializeToValue(
      st: SymbolicType,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    val reduced = SymbolicType.betaReduce(st)
    materializeReduced(reduced, substitution, source)
  }

  private def materializeReduced(
      st: SymbolicType,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    st match {
      case TypeReference(vfqn) =>
        Types.dataType(vfqn).pure[CompilerIO]

      case TypeVariable(name) =>
        substitution.get(name) match {
          case Some(value) => value.pure[CompilerIO]
          case None        => compilerAbort(source.as(s"Type expression contains unsubstituted parameter: $name"))
        }

      case LiteralType(value, typeFQN) =>
        Value.Direct(value, Types.dataType(typeFQN)).pure[CompilerIO]

      case TypeLambda(_, _, _) =>
        compilerAbort(source.as("Type expression reduced to unapplied type function"))

      case TypeApplication(_, _) =>
        val (constructor, args) = collectApplicationChain(st)
        for {
          resolvedConstructor <- resolveConstructor(constructor, substitution, source)
          materializedArgs    <- args.traverse(materializeReduced(_, substitution, source))
          result              <- applyChain(resolvedConstructor, materializedArgs, source)
        } yield result
    }

  /** Collect a left-nested TypeApplication chain into the base constructor and all arguments.
    * E.g. `App(App(FunctionRef, A), B)` → `(FunctionRef, Seq(A, B))`
    */
  private def collectApplicationChain(st: SymbolicType): (SymbolicType, Seq[SymbolicType]) =
    st match {
      case TypeApplication(target, arg) =>
        val (constructor, args) = collectApplicationChain(target.value)
        (constructor, args :+ arg.value)
      case other                        => (other, Seq.empty)
    }

  /** Resolve the base of a TypeApplication chain to an ExpressionValue constructor (typically a NativeFunction). */
  private def resolveConstructor(
      st: SymbolicType,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[ExpressionValue] =
    st match {
      case TypeReference(vfqn) =>
        getFact(NamedEvaluable.Key(vfqn)).flatMap {
          case Some(ev) => ev.value.pure[CompilerIO]
          case None     => compilerAbort(source.as(s"Could not resolve type constructor."), Seq(s"Not evaluable: ${vfqn.show}"))
        }

      case TypeVariable(name) =>
        substitution.get(name) match {
          case Some(Value.Structure(fields, Value.Type)) if isSimpleDataTypeRef(fields) =>
            getDataTypeVfqn(fields) match {
              case Some(vfqn) =>
                getFact(NamedEvaluable.Key(vfqn)).flatMap {
                  case Some(ev) => ev.value.pure[CompilerIO]
                  case None     =>
                    compilerAbort(source.as(s"Could not resolve type constructor for substituted variable: $name"))
                }
              case None       =>
                compilerAbort(source.as(s"Type variable $name does not resolve to a type constructor"))
            }
          case _ =>
            compilerAbort(source.as(s"Type variable $name does not resolve to a type constructor"))
        }

      case _ =>
        compilerAbort(source.as("Expected type constructor at base of type application"))
    }

  /** Apply a resolved constructor to a sequence of materialized Value arguments. */
  private def applyChain(
      constructor: ExpressionValue,
      args: Seq[Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    args
      .foldLeftM[CompilerIO, ExpressionValue](constructor) { (current, arg) =>
        current match {
          case ExpressionValue.NativeFunction(_, body) => body(arg).pure[CompilerIO]
          case _                                       =>
            compilerAbort(source.as("Cannot apply argument to non-function type constructor"))
        }
      }
      .flatMap {
        case ExpressionValue.ConcreteValue(v) => v.pure[CompilerIO]
        case _                                =>
          compilerAbort(source.as("Type constructor did not reduce to a concrete value"))
      }

  private def isSimpleDataTypeRef(fields: Map[String, Value]): Boolean =
    fields.size == 1 && fields.contains("$typeName")

  private def getDataTypeVfqn(fields: Map[String, Value]): Option[ValueFQN] =
    fields("$typeName") match {
      case Value.Direct(vfqn: ValueFQN, _) => Some(vfqn)
      case _                               => None
    }

}
