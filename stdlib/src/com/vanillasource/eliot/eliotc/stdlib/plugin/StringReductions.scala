package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{bigIntFQN, stringFQN}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Literal

import java.util.Locale
import java.util.regex.Matcher

/** The compile-time reductions for the `eliot.lang.String` operations — the compiler-track twin of the JVM backend's
  * `StringNatives`. Together they are what makes a string operation *platform behaviour the compiler can also run*: the
  * same `s.toUpperCase` answers a `where` guard or an `auto` bound while checking, and emits a `java.lang.String` call
  * when it survives to runtime.
  *
  * Each native reduces only when every argument is a concrete literal and otherwise stays stuck (a
  * [[SemValue.VStuckNative]]) on the FQN the residual call must name, so `Evaluator.renormalize` re-fires it once the
  * arguments concretise and the backend emits the leaf when they never do. A string that comes from `readLine`
  * therefore never "reduces" to anything here — it stays a call, exactly as intended.
  *
  * '''The two sides must agree, value for value.''' A divergence would mean a program computes one string while being
  * checked and another while running, which no later phase can catch. Three rules keep them equal and are the reason
  * the Scala below looks slightly laboured: case conversion pins [[Locale.ROOT]] (never the ambient default locale),
  * `substring`/`repeat` clamp their indices in `BigInt` — before any narrowing — exactly as the emitted bytecode does,
  * and every index is a '''code-point''' index. The last one is a language decision rather than a host detail: Eliot
  * fixes `length` and the index family in code points so they mean the same number on every target, while a Scala
  * `String` is UTF-16 like the JVM's, so `length`, `substring` and `indexOfInternal` translate here exactly as
  * `StringNatives` translates them in bytecode.
  */
object StringReductions {

  private def stringFqn(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName(name, Qualifier.Default))

  private val stringType: SemValue = VTopDef(stringFQN, None, Spine.SNil)
  private val intType: SemValue    = VTopDef(bigIntFQN, None, Spine.SNil)

  /** The exact-FQN string reductions, folded into [[StdlibNativesProcessor]]'s binding table. */
  val bindings: Map[ValueFQN, SemValue] = Map(
    unary("length")(s => intValue(codePointCount(s))),
    unary("isEmpty")(s => boolValue(s.isEmpty)),
    unary("isBlank")(s => boolValue(s.isBlank)),
    unary("trim")(s => stringValue(s.strip())),
    unary("toUpperCase")(s => stringValue(s.toUpperCase(Locale.ROOT))),
    unary("toLowerCase")(s => stringValue(s.toLowerCase(Locale.ROOT))),
    stringStringNative("startsWith")((prefix, s) => boolValue(s.startsWith(prefix))),
    stringStringNative("endsWith")((suffix, s) => boolValue(s.endsWith(suffix))),
    stringStringNative("contains")((part, s) => boolValue(s.contains(part))),
    stringStringNative("indexOfInternal")((part, s) => intValue(indexOfCodePoint(part, s))),
    unary("isInteger")(s => boolValue(s.matches(IntegerPattern))),
    substringNative,
    replaceNative,
    repeatNative,
    parseIntNative
  )

  /** The numerals `java.math.BigInteger` accepts — the same pattern the JVM backend's `isInteger` compiles in, and the
    * exact set `parseIntInternal` may be handed.
    */
  private val IntegerPattern = "[+-]?[0-9]+"

  /** The `(ability, method, dispatch type) -> native` table for the three `String` instances whose method is a leaf.
    * Each native is built from the impl-method FQN it is bound to, so its stuck form names that same method — the one
    * the JVM backend emits, and the one `Evaluator.renormalize` re-fires through.
    */
  val abilityImplNatives: Seq[(String, String, String, ValueFQN => SemValue)] = Seq(
    ("Eq", "equals", "String", implFqn => binaryString(implFqn)((a, b) => boolValue(a === b))),
    (
      "Compare",
      "lessThanOrEqual",
      "String",
      implFqn => binaryString(implFqn)((a, b) => boolValue(a.compareTo(b) <= 0))
    ),
    ("Combine", "combine", "String", implFqn => binaryString(implFqn)((a, b) => stringValue(a + b)))
  )

  // ----------------------------------------------------------------------------------------------------------------
  // The natives whose shape is not a plain string-to-value mapping
  // ----------------------------------------------------------------------------------------------------------------

  /** `substring(start, end, s)` — total by the same clamping the backend emits: both indices into `0..length`, with
    * `end` raised to `start` when it would precede it. Both are code-point indices, converted to the host's storage
    * offsets only once clamped, exactly as the emitted bytecode does.
    */
  private def substringNative: (ValueFQN, SemValue) = {
    val fqn = stringFqn("substring")
    fqn -> VNative(
      intType,
      start =>
        VNative(
          intType,
          end =>
            VNative(
              stringType,
              s =>
                (start, end, s) match {
                  case (ConcreteInt(from), ConcreteInt(to), ConcreteString(text)) =>
                    val count = codePointCount(text)
                    val lo    = clamp(from, count)
                    val hi    = clamp(to, count).max(lo)
                    stringValue(text.substring(storageOffset(text, lo), storageOffset(text, hi)))
                  case _                                                          => stuck(fqn, start, end, s)
                }
            )
        )
    )
  }

  /** `replace(target, replacement, s)` — every occurrence of `target` replaced literally, and the empty `target`
    * inserting `replacement` into every gap between code points, exactly as the emitted bytecode does. A host
    * `String.replace` would count a surrogate pair as two gaps, so that case goes through [[CodePointGaps]] with the
    * replacement quoted, since it sits in a regex position there.
    */
  private def replaceLiterally(target: String, replacement: String, text: String): String =
    if (target.isEmpty) text.replaceAll(CodePointGaps, Matcher.quoteReplacement(replacement))
    else text.replace(target, replacement)

  /** The string's start, and the position after each code point — the gaps an empty `target` replaces into. */
  private val CodePointGaps = "(?s)(?<=.)|^"

  private def replaceNative: (ValueFQN, SemValue) = {
    val fqn = stringFqn("replace")
    fqn -> VNative(
      stringType,
      target =>
        VNative(
          stringType,
          replacement =>
            VNative(
              stringType,
              s =>
                (target, replacement, s) match {
                  case (ConcreteString(t), ConcreteString(r), ConcreteString(text)) =>
                    stringValue(replaceLiterally(t, r, text))
                  case _                                                            => stuck(fqn, target, replacement, s)
                }
            )
        )
    )
  }

  /** `repeat(count, s)` — total by clamping `count` into `0..Integer.MAX_VALUE`, as the backend does. */
  private def repeatNative: (ValueFQN, SemValue) = {
    val fqn = stringFqn("repeat")
    fqn -> VNative(
      intType,
      count =>
        VNative(
          stringType,
          s =>
            (count, s) match {
              case (ConcreteInt(n), ConcreteString(text)) => stringValue(text.repeat(clamp(n, Int.MaxValue)))
              case _                                      => stuck(fqn, count, s)
            }
        )
    )
  }

  /** `parseIntInternal(s)` — the numeral `s` spells, read at full precision, or zero when `s` is not one.
    *
    * Total by the same zero the backend emits, and for the same reason: `parseInt` guards this leaf with an `if`, which
    * *builds* both arms though it runs only one, so a leaf that failed on a non-numeral would fail before the guard
    * chose. The zero can only be produced for the strings `parseInt` aborts on, so it never reaches a caller.
    */
  private def parseIntNative: (ValueFQN, SemValue) = {
    val fqn = stringFqn("parseIntInternal")
    fqn -> VNative(
      stringType,
      s =>
        s match {
          case ConcreteString(text) => bigIntValue(if (text.matches(IntegerPattern)) BigInt(text) else BigInt(0))
          case _                    => stuck(fqn, s)
        }
    )
  }

  // ----------------------------------------------------------------------------------------------------------------
  // Shapes
  // ----------------------------------------------------------------------------------------------------------------

  /** A one-argument `String -> A` reduction. */
  private def unary(name: String)(op: String => SemValue): (ValueFQN, SemValue) = {
    val fqn = stringFqn(name)
    fqn -> VNative(
      stringType,
      s =>
        s match {
          case ConcreteString(text) => op(text)
          case _                    => stuck(fqn, s)
        }
    )
  }

  /** A two-argument `String -> String -> A` reduction, stuck on its own FQN. */
  private def stringStringNative(name: String)(op: (String, String) => SemValue): (ValueFQN, SemValue) = {
    val fqn = stringFqn(name)
    fqn -> binaryString(fqn)(op)
  }

  /** A two-argument `String -> String -> A` reduction stuck on `residualFqn` — its own FQN for a plain leaf, the
    * implementation-method FQN for an ability leaf (whose FQN carries a resolution index and cannot be keyed
    * statically).
    */
  private def binaryString(residualFqn: ValueFQN)(op: (String, String) => SemValue): SemValue =
    VNative(
      stringType,
      a =>
        VNative(
          stringType,
          b =>
            (a, b) match {
              case (ConcreteString(x), ConcreteString(y)) => op(x, y)
              case _                                      => stuck(residualFqn, a, b)
            }
        )
    )

  /** The canonical stuck form: a [[SemValue.VStuckNative]] carrying the FQN the residual call must name and the
    * not-yet-concrete arguments, so it stays definitionally distinct and is re-fired once they concretise.
    */
  private def stuck(fqn: ValueFQN, args: SemValue*): SemValue =
    VStuckNative(fqn, args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  private object ConcreteString {
    def unapply(v: SemValue): Option[String] = v match {
      case VConst(GroundValue.Direct(Literal.StringValue(s), _)) => Some(s)
      case _                                                     => None
    }
  }

  private object ConcreteInt {
    def unapply(v: SemValue): Option[BigInt] = v match {
      case VConst(GroundValue.Direct(Literal.IntegerValue(i), _)) => Some(i)
      case _                                                      => None
    }
  }

  /** `min(max(index, 0), limit)` as a machine `Int`. The clamp runs in `BigInt`, so an index far outside the machine
    * range is bounded *before* it is narrowed and can never wrap into a valid position — the same order the emitted
    * bytecode uses.
    */
  private def clamp(index: BigInt, limit: Int): Int = index.max(BigInt(0)).min(BigInt(limit)).toInt

  /** The number of code points in `text` — what Eliot's `length` counts, and the unit its indices are in. A Scala
    * `String` is UTF-16, so this is not `text.length`.
    */
  private def codePointCount(text: String): Int = text.codePointCount(0, text.length)

  /** The UTF-16 offset of the code-point index `index` within `text`. Only called with an index already clamped into
    * `0..codePointCount`, which is what makes it total.
    */
  private def storageOffset(text: String, index: Int): Int = text.offsetByCodePoints(0, index)

  /** The code-point index of the first occurrence of `part` in `s`, or the host's negative "not found" index unchanged
    * — a sentinel is not a position, so it is not translated. The `private` Eliot leaf never exposes it: `indexOf`
    * turns it into an `{Abort}`.
    */
  private def indexOfCodePoint(part: String, s: String): Int = {
    val index = s.indexOf(part)
    if (index < 0) index else s.codePointCount(0, index)
  }

  private def stringValue(s: String): SemValue = VConst(GroundValue.Direct(s, Evaluator.stringGroundType))

  private def intValue(i: Int): SemValue = VConst(GroundValue.Direct(BigInt(i), Evaluator.bigIntGroundType))

  private def bigIntValue(i: BigInt): SemValue = VConst(GroundValue.Direct(i, Evaluator.bigIntGroundType))

  private def boolValue(b: Boolean): SemValue = if (b) Evaluator.trueValue else Evaluator.falseValue
}
