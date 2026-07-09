package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{bigIntFQN, boolFQN, stringFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding, GroundValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** The `stdlib` native contributor: emits the total [[ContributedBinding]] under [[StdlibNativesProcessor.stdlibLabel]]
  * for the stdlib functions whose reduction the compiler must supply for type-level computation but does not otherwise
  * reason about — the compile-time arithmetic behind `Int`'s dependent bounds ([[BigInteger]]'s `Arithmetic` ability
  * `add`/`subtract`/`multiply` and `inc`), the boolean operators (`&&`/`||`/`!`), string equality (`stringEquals`,
  * backing `Eq[String]`), and the [[BigInteger]] ordering comparison behind `Compare[BigInteger]` — and `None` for every
  * other name. (The `*` corner-product bounds `multiplyMin`/`multiplyMax` are ordinary Eliot bodies over
  * `Arithmetic.multiply` + `Compare.min`/`max`, not natives — they reduce through these leaves.)
  *
  * These are ordinary library functions; the compiler only seeds the NbE evaluator with a native reduction rule so that,
  * e.g., `add(2, 3)` reduces to `5` during type checking. Each native reduces only when its arguments are concrete,
  * otherwise it stays stuck (a [[SemValue.VStuckNative]]) so the unifier falls back to ordinary unification on the
  * still-abstract bounds and `Evaluator.renormalize` re-fires it once they concretise. The runtime computation (e.g.
  * the `LADD` for `Int` addition) is supplied separately by the backend as a runtime body — the
  * [[BindingMergerProcessor]] reads this native for checking and that body for codegen, with no conflict (native
  * precedence).
  *
  * '''`Compare` and `Arithmetic` — abilities reduced two ways.''' `BigInteger`'s comparison (`lessThanOrEqual`, the
  * `Compare` ability's method) and its arithmetic (`add`/`subtract`/`multiply`, the `Arithmetic` ability's methods) are all
  * implemented with *body-less* methods (`implement Compare[BigInteger]` / `implement Arithmetic[BigInteger, BigInteger]`),
  * so each is a compiler leaf. Every such native is registered under two FQNs: (1) the *ability-method* FQN
  * (`Compare.lessThanOrEqual`, `Arithmetic.add`, …) — the load-bearing binding, because `Int`'s bound calculus reaches
  * these during type-level reduction
  * (`add`/`subtract` directly in the `+`/`-` result types; `lessThanOrEqual` transitively through `fitsIn`/`min`/`max`),
  * where ability *dispatch* never fires, so on the compiler track the ability method itself must reduce (a compiler
  * intrinsic, like `Bool.fold`); and (2) the *implementation-method* FQN via [[abilityImplNativeFor]] — the "native
  * attached directly to the implementation" wiring, which a *value-level* instance (dispatched at a surfaced use site)
  * would use, but which `BigInteger`'s purely type-level use does not exercise. `min`/`max` are derived `Compare`
  * combinators (a `fold` over `lessThanOrEqual`), and `multiplyMin`/`multiplyMax` derive further over `multiply` +
  * `min`/`max`; all reduce through these leaf natives even when reached only transitively from a result type, because the
  * checker prefetches bindings transitively (`Checker.prefetchBindings`) so `renormalize` can re-fire the nested stuck
  * natives once the bound metavariables solve.
  *
  * This is a platform/library native supplier disjoint from the lang layer's `SystemNativesProcessor` (which owns the
  * compiler-intrinsic `Function`/`Type`/`Bool` primitives): each owns its own names, so the merger never has to choose
  * between two native answers. The plugin registers [[StdlibNativesProcessor.stdlibLabel]] in the merger's native
  * roster via `StdlibPlugin.configure()`. The well-known lang types it builds on (`bigIntFQN`/`boolFQN`) stay in the
  * lang layer; only these library reductions live here.
  */
class StdlibNativesProcessor extends SingleFactProcessor[ContributedBinding.Key] {

  private val bigIntegerModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "BigInteger")
  private val boolModule: ModuleName       = ModuleName(ModuleName.defaultSystemPackage, "Bool")
  private val eqModule: ModuleName         = ModuleName(ModuleName.defaultSystemPackage, "Eq")
  private val compareModule: ModuleName    = ModuleName(ModuleName.defaultSystemPackage, "Compare")
  private val arithmeticModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "Arithmetic")

  private val compareLessThanOrEqualFQN: ValueFQN =
    ValueFQN(compareModule, QualifiedName("lessThanOrEqual", Qualifier.Ability("Compare")))

  private def arithmeticFn(name: String): ValueFQN =
    ValueFQN(arithmeticModule, QualifiedName(name, Qualifier.Ability("Arithmetic")))

  private val arithmeticAddFQN: ValueFQN      = arithmeticFn("add")
  private val arithmeticSubtractFQN: ValueFQN = arithmeticFn("subtract")
  private val arithmeticMultiplyFQN: ValueFQN = arithmeticFn("multiply")

  private def bigIntegerFn(name: String): ValueFQN = ValueFQN(bigIntegerModule, QualifiedName(name, Qualifier.Default))

  private val incFQN: ValueFQN             = bigIntegerFn("inc")
  private val boolAndFQN: ValueFQN         = ValueFQN(boolModule, QualifiedName("&&", Qualifier.Default))
  private val boolOrFQN: ValueFQN          = ValueFQN(boolModule, QualifiedName("||", Qualifier.Default))
  private val boolNotFQN: ValueFQN         = ValueFQN(boolModule, QualifiedName("!", Qualifier.Default))
  private val stringEqualsFQN: ValueFQN    = ValueFQN(eqModule, QualifiedName("stringEquals", Qualifier.Default))

  private val bigIntType: SemValue = VTopDef(bigIntFQN, None, Spine.SNil)
  private val boolType: SemValue   = VTopDef(boolFQN, None, Spine.SNil)
  private val stringType: SemValue = VTopDef(stringFQN, None, Spine.SNil)

  private val bindings: Map[ValueFQN, SemValue] = Map(
    incFQN                    -> incNative,
    arithmeticAddFQN          -> addNative,
    arithmeticSubtractFQN     -> subtractNative,
    arithmeticMultiplyFQN     -> multiplyNative,
    boolAndFQN                -> andNative,
    boolOrFQN                 -> orNative,
    boolNotFQN                -> notNative,
    stringEqualsFQN           -> stringEqualsNative,
    compareLessThanOrEqualFQN -> lessThanOrEqualNative
  )

  /** `BigInteger`'s `Arithmetic` operations, each reducing to a concrete `BigInteger` when both operands are concrete
    * (otherwise stuck on the still-abstract `Int` bounds). Shared by the load-bearing ability-method [[bindings]] and the
    * value-level [[abilityImplNatives]] wiring.
    */
  private def addNative: SemValue      = bigIntBinaryNative(arithmeticAddFQN)((a, b) => a + b)
  private def subtractNative: SemValue = bigIntBinaryNative(arithmeticSubtractFQN)((a, b) => a - b)
  private def multiplyNative: SemValue = bigIntBinaryNative(arithmeticMultiplyFQN)((a, b) => a * b)

  override def generateSingleFact(key: ContributedBinding.Key): CompilerIO[ContributedBinding] =
    if (key.label =!= StdlibNativesProcessor.stdlibLabel) abort
    else
      bindings.get(key.vfqn) match {
        case some @ Some(_) => ContributedBinding(key.vfqn, key.label, some.map(BindingContribution.Leaf(_))).pure[CompilerIO]
        case None           =>
          abilityImplNativeFor(key.vfqn).map(sem => ContributedBinding(key.vfqn, key.label, sem.map(BindingContribution.Leaf(_))))
      }

  /** The natives attached *directly* to an ability-implementation method rather than to a plain `Default`-qualified leaf.
    * An impl method's FQN carries a per-module `index` assigned during resolution, so it cannot be keyed statically in
    * [[bindings]]; instead it is recognised by `(ability, method, dispatch type)` through the impl marker
    * ([[ImplementationMarkerUtils.isImplementationMethodFor]]), matched against [[abilityImplNatives]]: the
    * `Compare[BigInteger].lessThanOrEqual` comparison and the `Arithmetic[BigInteger, BigInteger]` `add`/`subtract`/`multiply`
    * arithmetic behind `Int`'s dependent ranges. Further ability-native leaves are added there.
    */
  private def abilityImplNativeFor(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    abilityImplNatives.toList
      .traverse { case (ability, method, dispatchType, native) =>
        ImplementationMarkerUtils.isImplementationMethodFor(vfqn, ability, method, dispatchType).map(Option.when(_)(native))
      }
      .map(_.flatten.headOption)

  /** The `(ability, method, dispatch type) -> native` table backing [[abilityImplNativeFor]]. Each native is the same
    * one bound under its ability-method FQN in [[bindings]]; this is the value-level dispatch path (a surfaced use site),
    * which `BigInteger`'s purely type-level use does not exercise but which keeps the two wirings in lockstep.
    */
  private def abilityImplNatives: Seq[(String, String, String, SemValue)] = Seq(
    ("Compare", "lessThanOrEqual", "BigInteger", lessThanOrEqualNative),
    ("Arithmetic", "add", "BigInteger", addNative),
    ("Arithmetic", "subtract", "BigInteger", subtractNative),
    ("Arithmetic", "multiply", "BigInteger", multiplyNative)
  )

  /** The canonical stuck form of a native: a [[SemValue.VStuckNative]] carrying the native's own FQN and the
    * (not-yet-concrete) arguments as its spine — so it stays definitionally distinct, is re-fired by
    * `Evaluator.renormalize` once concrete, and is never injectivity-decomposed by the unifier.
    */
  private def stuck(fqn: ValueFQN, args: SemValue*): SemValue =
    VStuckNative(fqn, args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  /** `inc(n: BigInteger): BigInteger` — reduces `inc(VConst(Direct(n, _)))` to `VConst(Direct(n+1, _))`. */
  private def incNative: SemValue =
    VNative(
      bigIntType,
      {
        case VConst(GroundValue.Direct(n: BigInt, tpe)) => VConst(GroundValue.Direct(n + 1, tpe))
        case other                                      => stuck(incFQN, other)
      }
    )

  /** `lessThanOrEqual(a, b): Bool` — reduces to a concrete Bool when both arguments are concrete BigIntegers, otherwise
    * stays stuck (so the unifier falls back to ordinary unification).
    */
  private def lessThanOrEqualNative: SemValue =
    VNative(bigIntType, a => VNative(bigIntType, b => lessThanOrEqualResult(a, b)))

  private def lessThanOrEqualResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: BigInt, _)), VConst(GroundValue.Direct(y: BigInt, _))) =>
      if (x <= y) Evaluator.trueValue else Evaluator.falseValue
    case _                                                                                    =>
      stuck(compareLessThanOrEqualFQN, a, b)
  }

  /** A curried `BigInteger -> BigInteger -> BigInteger` native (e.g. `add`/`subtract`): reduces to a
    * concrete BigInteger when both arguments are concrete, otherwise stays stuck on the still-abstract bounds.
    */
  private def bigIntBinaryNative(fqn: ValueFQN)(op: (BigInt, BigInt) => BigInt): SemValue =
    VNative(bigIntType, a => VNative(bigIntType, b => bigIntBinaryResult(fqn, op, a, b)))

  private def bigIntBinaryResult(fqn: ValueFQN, op: (BigInt, BigInt) => BigInt, a: SemValue, b: SemValue): SemValue =
    (a, b) match {
      case (VConst(GroundValue.Direct(x: BigInt, t)), VConst(GroundValue.Direct(y: BigInt, _))) =>
        VConst(GroundValue.Direct(op(x, y), t))
      case _                                                                                    =>
        stuck(fqn, a, b)
    }

  /** `&&(a, b)`: reduces to `Direct(a && b)` when both arguments are concrete Bools, otherwise stays stuck. */
  private def andNative: SemValue =
    VNative(boolType, a => VNative(boolType, b => andResult(a, b)))

  private def andResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: Boolean, _)), VConst(GroundValue.Direct(y: Boolean, _))) =>
      VConst(GroundValue.Direct(x && y, Evaluator.boolGroundType))
    case _                                                                                      =>
      stuck(boolAndFQN, a, b)
  }

  /** `||(a, b)`: reduces to `Direct(a || b)` when both arguments are concrete Bools, otherwise stays stuck. */
  private def orNative: SemValue =
    VNative(boolType, a => VNative(boolType, b => orResult(a, b)))

  private def orResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: Boolean, _)), VConst(GroundValue.Direct(y: Boolean, _))) =>
      VConst(GroundValue.Direct(x || y, Evaluator.boolGroundType))
    case _                                                                                      =>
      stuck(boolAndFQN, a, b)
  }

  /** `!a`: reduces to `Direct(!a)` when argument is a concrete Bools, otherwise stays stuck. */
  private def notNative: SemValue =
    VNative(boolType, a => notResult(a))

  private def notResult(a: SemValue): SemValue = a match {
    case VConst(GroundValue.Direct(x: Boolean, _)) =>
      VConst(GroundValue.Direct(!x, Evaluator.boolGroundType))
    case _                                         =>
      stuck(boolAndFQN, a)
  }

  /** `stringEquals(a, b): Bool` — the compile-time counterpart of the jvm `stringEquals` leaf (backing `Eq[String]`).
    * Reduces to a concrete Bool when both arguments are concrete strings, otherwise stays stuck (a runtime string,
    * e.g. a `readLine` result, keeps the comparison as a residual call for the backend to emit).
    */
  private def stringEqualsNative: SemValue =
    VNative(stringType, a => VNative(stringType, b => stringEqualsResult(a, b)))

  private def stringEqualsResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: String, _)), VConst(GroundValue.Direct(y: String, _))) =>
      if (x === y) Evaluator.trueValue else Evaluator.falseValue
    case _                                                                                    =>
      stuck(stringEqualsFQN, a, b)
  }
}

object StdlibNativesProcessor {

  /** This contributor's native-category label in the [[ContributedBinding]] merge. `StdlibPlugin.configure()` adds it
    * to the merger's extra-native roster ([[ContributedBinding.extraNativeLabelsKey]]); tests that compose this
    * processor onto `LangProcessors` pass it as `extraNativeBindingLabels`.
    */
  val stdlibLabel: String = "stdlib"
}
