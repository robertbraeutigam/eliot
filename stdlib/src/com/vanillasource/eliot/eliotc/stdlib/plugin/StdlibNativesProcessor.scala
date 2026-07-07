package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.syntax.all.*
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
  * reason about — the compile-time arithmetic/comparison on [[BigInteger]] backing `Int`'s dependent bounds
  * (`add`/`subtract`/`min`/`max`/`multiplyMin`/`multiplyMax`/ `lessThanOrEqual`), the boolean operators (`&&`/`||`/`!`),
  * string equality (`stringEquals`, backing `Eq[String]`), and `inc` — and `None` for every other name.
  *
  * These are ordinary library functions (declared body-less in the stdlib layer's `BigInteger.els`/`Bool.els`); the
  * compiler only seeds the NbE evaluator with a native reduction rule so that, e.g., `add(2, 3)` reduces to `5` and
  * `lessThanOrEqual(0, 1)` to `true` during type checking. Each native reduces only when its arguments are concrete,
  * otherwise it stays stuck (a [[SemValue.VStuckNative]]) so the unifier falls back to ordinary unification on the
  * still-abstract bounds and `Evaluator.renormalize` re-fires it once they concretise. The runtime computation (e.g.
  * the `LADD` for `Int` addition) is supplied separately by the backend as a runtime body — the
  * [[BindingMergerProcessor]] reads this native for checking and that body for codegen, with no conflict (native
  * precedence).
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

  private def bigIntegerFn(name: String): ValueFQN = ValueFQN(bigIntegerModule, QualifiedName(name, Qualifier.Default))

  private val incFQN: ValueFQN             = bigIntegerFn("inc")
  private val lessThanOrEqualFQN: ValueFQN = bigIntegerFn("lessThanOrEqual")
  private val minFQN: ValueFQN             = bigIntegerFn("min")
  private val maxFQN: ValueFQN             = bigIntegerFn("max")
  private val addFQN: ValueFQN             = bigIntegerFn("add")
  private val subtractFQN: ValueFQN        = bigIntegerFn("subtract")
  private val multiplyMinFQN: ValueFQN     = bigIntegerFn("multiplyMin")
  private val multiplyMaxFQN: ValueFQN     = bigIntegerFn("multiplyMax")
  private val boolAndFQN: ValueFQN         = ValueFQN(boolModule, QualifiedName("&&", Qualifier.Default))
  private val boolOrFQN: ValueFQN          = ValueFQN(boolModule, QualifiedName("||", Qualifier.Default))
  private val boolNotFQN: ValueFQN         = ValueFQN(boolModule, QualifiedName("!", Qualifier.Default))
  private val stringEqualsFQN: ValueFQN    = ValueFQN(eqModule, QualifiedName("stringEquals", Qualifier.Default))

  private val bigIntType: SemValue = VTopDef(bigIntFQN, None, Spine.SNil)
  private val boolType: SemValue   = VTopDef(boolFQN, None, Spine.SNil)
  private val stringType: SemValue = VTopDef(stringFQN, None, Spine.SNil)

  private val bindings: Map[ValueFQN, SemValue] = Map(
    incFQN             -> incNative,
    lessThanOrEqualFQN -> lessThanOrEqualNative,
    minFQN             -> bigIntBinaryNative(minFQN)((a, b) => a min b),
    maxFQN             -> bigIntBinaryNative(maxFQN)((a, b) => a max b),
    addFQN             -> bigIntBinaryNative(addFQN)((a, b) => a + b),
    subtractFQN        -> bigIntBinaryNative(subtractFQN)((a, b) => a - b),
    multiplyMinFQN     -> bigIntCornerNative(multiplyMinFQN)(_.min),
    multiplyMaxFQN     -> bigIntCornerNative(multiplyMaxFQN)(_.max),
    boolAndFQN         -> andNative,
    boolOrFQN          -> orNative,
    boolNotFQN         -> notNative,
    stringEqualsFQN    -> stringEqualsNative
  )

  override def generateSingleFact(key: ContributedBinding.Key): CompilerIO[ContributedBinding] =
    if (key.label =!= StdlibNativesProcessor.stdlibLabel) abort
    else
      ContributedBinding(key.vfqn, key.label, bindings.get(key.vfqn).map(BindingContribution.Leaf(_))).pure[CompilerIO]

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
      stuck(lessThanOrEqualFQN, a, b)
  }

  /** A curried `BigInteger -> BigInteger -> BigInteger` native (e.g. `min`/`max`/`add`/`subtract`): reduces to a
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

  /** A curried 4-argument `BigInteger -> … -> BigInteger` native over the corner products of `Int[a,b] * Int[c,d]`
    * (`multiplyMin`/`multiplyMax`): when all four bounds are concrete it reduces to `op(Seq(a*c, a*d, b*c, b*d))`
    * (`min`/`max`), otherwise it stays stuck.
    */
  private def bigIntCornerNative(fqn: ValueFQN)(op: Seq[BigInt] => BigInt): SemValue = {
    def collect(acc: Seq[SemValue], remaining: Int): SemValue =
      if (remaining === 0) bigIntCornerResult(fqn, op, acc)
      else VNative(bigIntType, arg => collect(acc :+ arg, remaining - 1))
    collect(Seq.empty, 4)
  }

  private def bigIntCornerResult(fqn: ValueFQN, op: Seq[BigInt] => BigInt, args: Seq[SemValue]): SemValue = args match {
    case Seq(
          VConst(GroundValue.Direct(a: BigInt, t)),
          VConst(GroundValue.Direct(b: BigInt, _)),
          VConst(GroundValue.Direct(c: BigInt, _)),
          VConst(GroundValue.Direct(d: BigInt, _))
        ) =>
      VConst(GroundValue.Direct(op(Seq(a * c, a * d, b * c, b * d)), t))
    case _ =>
      stuck(fqn, args*)
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
