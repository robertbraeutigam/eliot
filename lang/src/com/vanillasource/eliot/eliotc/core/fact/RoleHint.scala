package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.module.fact.QualifiedName

/** Advisory metadata about how a [[NamedValue]] was declared. Populated only by phases that have structural knowledge
  * of the declaration (e.g. data-definition desugaring); defaults to [[RoleHint.NoHint]] for parsed user code.
  *
  * Hints are intended for phases that synthesize code matching the programmer's written shape (e.g. match desugaring,
  * future kind-directed type variable insertion). Semantic phases (type checking, monomorphization) must not consult
  * the hint — they work from signatures alone. The tagged ADT is deliberate: any consumer must commit to a specific
  * role to extract its information, which prevents accidental reuse of a count from one role in another.
  */
sealed trait RoleHint

object RoleHint {

  /** No structural information available. The default for parsed user code and any synthetic value whose role is not
    * tracked.
    */
  case object NoHint extends RoleHint

  /** A value constructor synthesized from a `data` declaration.
    *
    * @param dataType
    *   The local qualified name of the type constructor this value belongs to (with [[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Type]]).
    *   Carried as a [[QualifiedName]] rather than a full `ValueFQN` because the constructor's module is always the
    *   same as the data type's module — that invariant lets consumers derive the module from the constructor itself.
    * @param fieldCount
    *   Number of fields the programmer declared on this constructor.
    */
  case class ValueConstructor(dataType: QualifiedName, fieldCount: Int) extends RoleHint

  /** A type constructor synthesized from a `data` declaration.
    *
    * @param typeParamCount
    *   Number of generic parameters the programmer declared. Equivalent to "how many arguments must be applied to
    *   reach `Type`," which is what kind-directed saturation needs.
    */
  case class TypeConstructor(typeParamCount: Int) extends RoleHint
}
