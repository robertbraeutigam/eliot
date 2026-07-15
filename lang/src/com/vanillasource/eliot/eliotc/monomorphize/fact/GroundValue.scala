package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN, WellKnownTypes}

/** Ground values represent fully evaluated, concrete values with no free variables or unsolved metas. These are the
  * output of quoting NbE semantic values back to a first-order representation.
  *
  * Examples:
  *   - `Direct(42, bigIntType)` — the integer 42 with type BigInteger
  *   - `Structure(functionFQN, Seq(intType, stringType), Type)` — Function[Int, String]
  *   - `Type` — the type of all types
  */
sealed trait GroundValue {
  def valueType: GroundValue
}

object GroundValue {
  case class Direct(value: Any, override val valueType: GroundValue) extends GroundValue

  /** A type-constructor or data-constructor application identified by [[typeName]] applied to positional [[args]]. */
  case class Structure(typeName: ValueFQN, args: Seq[GroundValue], override val valueType: GroundValue)
      extends GroundValue

  /** The type of all types and Type itself. This is the root of the type hierarchy where the infinite recursion stops.
    */
  object Type extends GroundValue {
    override def valueType: GroundValue = this
  }

  /** A signature-twin *parameter* (signature-unification C2): a leftover generic binder that a *partial-arity* signature
    * twin received no type argument for, read back **under its binder** rather than defaulted to `Type`. `index` is the
    * binder's position; `args` its applied spine (a higher-kinded binder `F[_]` occurring as `F[Unit]` carries `[Unit]`).
    *
    * A `Param` lives **only** inside a `CompilerMonomorphicValue(*@Signature)` fact at a partial key — it is the honest
    * read-back of a generic signature (a function value) at fewer arguments than binders. Its two consumers:
    *   - the value mono re-inflates each `Param` to a *fresh metavariable* (`Evaluator.groundToSem` with a Param→meta
    *     substitution), preserving the constraint-covered deferral a leftover binder needs (signature-unification §4.7);
    *   - `groundToSem` **without** a substitution hard-errors on a `Param` (fail-safe) — a `Param` must never leak into a
    *     value mono's own published signature (codegen's erased/defaulted form, finding 2) or reach the runtime track.
    */
  case class Param(index: Int, args: Seq[GroundValue], override val valueType: GroundValue) extends GroundValue

  extension (gv: GroundValue) {

    def typeFQN: Option[ValueFQN] =
      gv match {
        case Structure(name, _, _) => Some(name)
        case Type                  => Some(WellKnownTypes.typeFQN)
        case _                     => None
      }

    def asFunctionType: Option[(GroundValue, GroundValue)] =
      gv match {
        case Structure(name, Seq(a, b), Type) if name === WellKnownTypes.functionDataTypeFQN => Some((a, b))
        case _                                                                               => None
      }

    @scala.annotation.tailrec
    def deepReturnType: GroundValue =
      gv.asFunctionType match {
        case Some((_, returnType)) => returnType.deepReturnType
        case None                  => gv
      }

    def functionArity: Int =
      gv.asFunctionType match {
        case Some((_, returnType)) => 1 + returnType.functionArity
        case None                  => 0
      }

    def extractParamAndReturnTypes: (Seq[GroundValue], GroundValue) =
      gv.asFunctionType match {
        case Some((paramType, returnType)) =>
          val (restParams, finalReturn) = returnType.extractParamAndReturnTypes
          (paramType +: restParams, finalReturn)
        case None                          =>
          (Seq.empty, gv)
      }

    /** The runtime *carrier* FQN this value's type erases to — the platform-independent half of type erasure. Function
      * types collapse to [[WellKnownTypes.functionCarrierFQN]]; a concrete data/structure type keeps its FQN with the
      * type-constructor qualifier stripped to [[Qualifier.Default]]; everything else (`Type` itself, or an
      * erased/phantom value with no type FQN) collapses to the opaque top carrier [[WellKnownTypes.anyFQN]]. A backend
      * maps the resulting FQN to a concrete machine type (the JVM via `NativeType.types`); this method holds the
      * collapse logic so no backend reimplements it.
      */
    def carrierFQN: ValueFQN =
      gv match {
        case _ if gv.asFunctionType.isDefined => WellKnownTypes.functionCarrierFQN
        case _                                =>
          gv.typeFQN match {
            case Some(name) if name =!= WellKnownTypes.typeFQN =>
              ValueFQN(name.moduleName, QualifiedName(name.name.name, Qualifier.Default))
            case _                                             => WellKnownTypes.anyFQN
          }
      }
  }

  given Eq[GroundValue] = Eq.fromUniversalEquals

  given Show[GroundValue] = {
    case Type                                     => "Type"
    case Direct(value, _)                         => value.toString
    case Structure(name, _, valueType) if valueType === Type => name.name.name
    case Structure(_, _, _)                       => "Structure(...)"
    case Param(index, Nil, _)                     => s"?p$index"
    case Param(index, args, _)                    => s"?p$index[${args.map(_.show).mkString(", ")}]"
  }
}
