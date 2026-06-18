package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.module.fact.QualifiedName

/** Advisory metadata about how a [[NamedValue]] was declared. Populated only by phases that have structural knowledge
  * of the declaration (e.g. data-definition desugaring); defaults to [[RoleHint.NoHint]] for parsed user code.
  *
  * Hints are intended for phases that synthesize code matching the programmer's written shape (e.g. match desugaring,
  * future kind-directed type variable insertion). The tagged ADT is deliberate: any consumer must commit to a specific
  * role to extract its information, which prevents accidental reuse of a count from one role in another.
  *
  * '''Cornerstone invariant (types-are-values / λ\*).''' No phase may use a hint to make a ''typing'' decision — in
  * particular nothing may read [[TypeConstructor.typeParamCount]] (or otherwise treat a parameter as "a type
  * parameter" rather than an ordinary parameter); the type checker / monomorphization work from signatures alone and
  * type equality is definitional. That arity/kind distinction is exactly the stratification the cornerstone denies, so
  * `typeParamCount` is deliberately ''write-only today'' (populated by the desugarer, read by no one — verified by the
  * cornerstone-fidelity Phase 3 audit). The ''only'' sanctioned reads of a hint are shape
  * reconstruction for `match`: `DataMatchDesugarer` (the syntactic half) and `monomorphize/.../MatchNativesProcessor`
  * (the native-emitting half) consult [[ValueConstructor]] purely to recover a data type's constructors, their
  * declaration order, and their field arity when baking the pattern-dispatch native — never to distinguish type-level
  * from value-level. Reading `ValueConstructor` for that purpose is allowed even from the monomorphize package;
  * reading `typeParamCount`, or branching a typing rule on a hint, is not.
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
    *   reach `Type`," which is what kind-directed saturation needs. Currently '''unread''' — reserved for a future
    *   syntactic/desugaring use only; per the cornerstone invariant above no semantic phase may consult it.
    */
  case class TypeConstructor(typeParamCount: Int) extends RoleHint

  /** A field accessor synthesized from a single-constructor `data` declaration (`data Counter(n: Int)` ⇒ `def
    * n(obj: Counter): Int`). Records which field of which data type the accessor projects, so the implicit-generics
    * data-field saturation (`docs/implicit-generics-plan.md`, W2) can correlate the accessor's return bounds with the
    * data type's synthesized binders — the field order is not otherwise recoverable from the (abstract, name-less)
    * value-constructor signature. This is an '''elaboration''' read (it shapes which bounds are filled in, never a
    * typing decision), the same flavour as the sanctioned [[ValueConstructor]] reads in match-shape reconstruction.
    *
    * @param dataType
    *   The local qualified name of the data type this accessor belongs to (with
    *   [[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Type]]).
    * @param fieldIndex
    *   Zero-based position of the projected field in the constructor's declaration order.
    */
  case class FieldAccessor(dataType: QualifiedName, fieldIndex: Int) extends RoleHint
}
