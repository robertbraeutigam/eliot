package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** The JVM backend's `Int` width policy: decode a refinement-channel value-range meta (an `Interval[BigInteger]`
  * [[GroundValue]], the `[min, max]` the channel pinned for a node) into the machine layout that carries it — the
  * smallest signed wrapper whose width contains the interval, falling back to a bignum.
  *
  * This is the Scala successor of the Eliot `Represent[Interval]` instance (`jvm/eliot-compiler/.../Represent.els`): the
  * `interval → width` decision is JVM-specific codegen knowledge (an ATtiny backend would map the same interval to
  * different widths), so it belongs in the backend, not in an Eliot ability the checker evaluates. The thresholds are the
  * standard signed machine-integer bounds — the exact `fitsByte/fitsShort/fitsMedium/fitsLong` fold `Represent.els`
  * performed (`stdlib/.../BigInteger.els`), kept byte-for-identical so the move is behaviour-preserving. See
  * `docs/generic-refinement-merges.md` (Step 6) and `docs/bounds-as-refinements.md`.
  *
  * The output is a `Jvm*` representation-type [[ValueFQN]] the rest of the backend already understands
  * ([[NativeType.jvmRepresentationType]] → [[NativeType.types]]); the `Jvm*` names stay an internal backend vocabulary.
  */
object IntRepresentation {
  private val byteMin   = BigInt(-128)
  private val byteMax   = BigInt(127)
  private val shortMin  = BigInt(-32768)
  private val shortMax  = BigInt(32767)
  private val mediumMin = BigInt(-2147483648L)
  private val mediumMax = BigInt(2147483647L)
  private val longMin   = BigInt(Long.MinValue)
  private val longMax   = BigInt(Long.MaxValue)

  private val jvmByte       = NativeType.jvmRepresentationType("JvmByte")
  private val jvmShort      = NativeType.jvmRepresentationType("JvmShort")
  private val jvmInt        = NativeType.jvmRepresentationType("JvmInt")
  private val jvmLong       = NativeType.jvmRepresentationType("JvmLong")
  private val jvmBigInteger = NativeType.jvmRepresentationType("JvmBigInteger")

  /** The machine layout carrying every value in `[min, max]`: the narrowest signed wrapper that contains it, else a
    * bignum. Matches `Represent.els`'s `fitsByte → JvmByte`, …, `fitsLong → JvmLong`, else `JvmBigInteger` fold exactly.
    */
  def widthOf(min: BigInt, max: BigInt): ValueFQN =
    if (byteMin <= min && max <= byteMax) jvmByte
    else if (shortMin <= min && max <= shortMax) jvmShort
    else if (mediumMin <= min && max <= mediumMax) jvmInt
    else if (longMin <= min && max <= longMax) jvmLong
    else jvmBigInteger

  /** The `eliot.lang.Int` module — home of both the tracked `Int` type and the `Jvm*` representation types. */
  private val intModuleName: ModuleName = ModuleName(defaultSystemPackage, "Int")

  private val integerTypeNames: Set[String] =
    Set("Int", "JvmByte", "JvmShort", "JvmInt", "JvmLong", "JvmBigInteger")

  /** Whether `gv` is an integer-carrying type — the tracked `Int` (post-flag-day, its width comes from the channel meta)
    * or one of the lowered `Jvm*` representation types (pre-un-lowering). Both live in the `eliot.lang.Int` module; the
    * qualifier is ignored (a type head may carry `Type` or the stripped `Default`). A node of such a type has its machine
    * width decided by its refinement meta ([[representationFor]]); any other type is laid out by [[NativeType]] as usual.
    */
  def isIntegerType(gv: GroundValue): Boolean = gv match {
    case GroundValue.Structure(fqn, _, _) =>
      fqn.moduleName == intModuleName && integerTypeNames.contains(fqn.name.name)
    case _                                => false
  }

  /** The representation for a channel meta: decode its interval when present, else the ⊤/unknown layout (`JvmBigInteger`)
    * — a value the channel could not pin is soundly a bignum. A non-`Interval` meta (a future domain) also yields the ⊤
    * layout here: this backend only knows how to lay out the value-range domain.
    */
  def representationFor(meta: Option[GroundValue]): ValueFQN =
    meta.flatMap(intervalBounds).map { case (min, max) => widthOf(min, max) }.getOrElse(jvmBigInteger)

  /** Extract `[min, max]` from an `Interval(min, max)` meta value — a two-field [[GroundValue.Structure]] whose fields are
    * `Direct` big integers. [[None]] for any other shape (a non-interval domain, a malformed meta), which the caller
    * treats as ⊤.
    */
  def intervalBounds(meta: GroundValue): Option[(BigInt, BigInt)] =
    meta match {
      case GroundValue.Structure(_, Seq(lo, hi), _) => (directBigInt(lo), directBigInt(hi)) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
      case _                                        => None
    }

  private def directBigInt(gv: GroundValue): Option[BigInt] = gv match {
    case GroundValue.Direct(value, _) =>
      value match {
        case b: BigInt               => Some(b)
        case b: java.math.BigInteger => Some(BigInt(b))
        case i: Int                  => Some(BigInt(i))
        case l: Long                 => Some(BigInt(l))
        case _                       => None
      }
    case _                            => None
  }
}
