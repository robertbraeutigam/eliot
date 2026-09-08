package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  listAppendFQN,
  listEmptyFQN,
  listFQN,
  listFoldLeftInternalFQN,
  listPrependFQN,
  listSplitFQN,
  listWordsFQN,
  stringFQN
}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Literal

import java.util.regex.Pattern

/** The compile-time twins of `eliot.collection.List`'s platform leaves — the compiler-track counterpart of the JVM
  * backend's `java.util.List` natives (`empty`/`append`/`prepend`/`foldLeftInternal`) and of the two string-splitting
  * leaves that answer a list (`split`/`words`, realised in `StringNatives`). With these a list computation is
  * *platform behaviour the compiler can also run*: an ability guard or a `where` over a literal list reduces while
  * checking, and the same operation emits a `java.util.List` call when it survives to runtime.
  *
  * '''The compile-time list has no representation of its own.''' `List` is abstract in the base (it commits to no
  * layout, not even on the compiler platform), so a concrete list here is its *normal form*: a chain of `prepend`
  * applications over `empty`, both left as the body-less constructor applications they already are —
  * `prepend(prepend(empty, b), a)` is the list `[a, b]`. `empty` and `prepend` are therefore their own reductions (a
  * [[SemValue.VTopDef]] with no body, which the unifier decomposes injectively and the quoter reads back structurally,
  * exactly like a `data` constructor), and the other four leaves reduce *over* that chain: `append` rebuilds it with
  * one more element at the back, `foldLeftInternal` walks it front to back applying the combining function,
  * `split`/`words` build one from a concrete string. Nothing new is added to the ground domain (`GroundValue.Literal`
  * stays the closed set it is) and nothing is materialised: a chain reaching read-back declines materialisation the
  * same way it does today, so codegen is untouched and still emits the structural `append`/`prepend` calls.
  *
  * Each reduction fires only when its list argument is a concrete chain (and, for the string leaves, its string a
  * literal), and otherwise stays stuck (a [[SemValue.VStuckNative]]) on the FQN the residual call must name, so
  * `Evaluator.renormalize` re-fires it once the arguments concretise and the backend emits the leaf when they never
  * do. A list built from a `readLine` result is never a chain and so never "reduces" here — it stays a call, exactly
  * as intended. The *elements* of a chain may be anything, including still-abstract terms: `isEmpty(prepend(empty,
  * x))` is `false` whatever `x` is, and a fold's combining function is applied to the element as it stands.
  *
  * '''The two sides must agree, value for value.''' `split` and `words` are the same `java.lang.String` calls the
  * backend emits — `Pattern.quote` and limit `-1` for a literal separator, the code-point boundary with its trailing
  * empty piece dropped for the empty one, `strip` then a whitespace-run split for `words`, with a blank string
  * branched out to the empty list — so a program computes the same pieces while being checked and while running.
  */
object ListReductions {

  private val listType: SemValue   = VTopDef(listFQN, None, Spine.SNil)
  private val stringType: SemValue = VTopDef(stringFQN, None, Spine.SNil)

  /** The exact-FQN list reductions, folded into [[SystemNativesProcessor]]'s system reductions. */
  val bindings: Map[ValueFQN, SemValue] = Map(
    listEmptyFQN            -> emptyList,
    listPrependFQN          -> prependNative,
    listAppendFQN           -> appendNative,
    listFoldLeftInternalFQN -> foldLeftInternalNative,
    listSplitFQN            -> splitNative,
    listWordsFQN            -> wordsNative
  )

  /** `empty` reduces to itself: the constructor application heading every chain. A written type argument
    * (`empty[Int]`) is applied onto its spine as for any body-less definition, so the node keeps the list's element type.
    */
  private def emptyList: SemValue = VTopDef(listEmptyFQN, None, Spine.SNil)

  /** `prepend(list, element)` reduces to its own constructor application — the canonical cell of the chain, always
    * carrying exactly the two value arguments (a written type argument is not modelled by the native and is dropped, as
    * for every generic leaf).
    */
  private def prependNative: SemValue =
    VNative(listType, list => VNative(VType, element => cell(list, element)))

  private def cell(list: SemValue, element: SemValue): SemValue =
    VTopDef(listPrependFQN, None, Spine.SNil :+ list :+ element)

  /** `append(list, element)` — the chain rebuilt with `element` at the back, over the same `empty` node the input
    * chain ends in. Stuck on its own FQN while `list` is not a concrete chain.
    */
  private def appendNative: SemValue =
    VNative(
      listType,
      list =>
        VNative(
          VType,
          element =>
            list match {
              case Chain(empty, elements) => chain(empty, elements :+ element)
              case _                      => stuck(listAppendFQN, list, element)
            }
        )
    )

  /** `foldLeftInternal(list, initial, combine)` — `combine(e)(acc)` applied over the elements from front to back,
    * starting at `initial`: `append(append(empty, a), b)` folds to `combine(b)(combine(a)(initial))`. The applications
    * are ordinary [[Evaluator.applyValue]]s, so an application that cannot reduce yet (a combining function whose body
    * is still stuck on an unresolved ability) simply stays as it is inside the result — a partially reduced term, never
    * a wrong one. Stuck on its own FQN while `list` is not a concrete chain.
    */
  private def foldLeftInternalNative: SemValue =
    VNative(
      listType,
      list =>
        VNative(
          VType,
          initial =>
            VNative(
              VType,
              combine =>
                list match {
                  case Chain(_, elements) =>
                    elements.foldLeft(initial)((acc, e) => Evaluator.applyValue(Evaluator.applyValue(combine, e), acc))
                  case _                  => stuck(listFoldLeftInternalFQN, list, initial, combine)
                }
            )
        )
    )

  /** The separator `words` splits on: any run of whitespace — the backend's `WhitespaceRun`. */
  private val WhitespaceRun = "\\s+"

  /** What the *empty* separator cuts `split` at: every code-point boundary — the backend's `CodePointBoundary`, so
    * `split("", s)` is the code points of `s`, never the halves of a surrogate pair.
    */
  private val CodePointBoundary = "(?s)(?<=.)"

  /** `split(separator, s)` — the pieces of `s` between literal occurrences of `separator`, every empty piece kept; the
    * empty separator cuts at every code point (its one trailing empty piece dropped, so `split("", "")` is empty).
    */
  private def splitNative: SemValue =
    VNative(
      stringType,
      separator =>
        VNative(
          stringType,
          s =>
            (separator, s) match {
              case (ConcreteString(sep), ConcreteString(text)) => stringList(splitLiterally(sep, text))
              case _                                           => stuck(listSplitFQN, separator, s)
            }
        )
    )

  private def splitLiterally(separator: String, text: String): Seq[String] =
    if (separator.isEmpty) text.split(CodePointBoundary, -1).toSeq.dropRight(1)
    else text.split(Pattern.quote(separator), -1).toSeq

  /** `words(s)` — the non-empty whitespace-separated pieces of `s`; a blank string has no words at all. */
  private def wordsNative: SemValue =
    VNative(
      stringType,
      s =>
        s match {
          case ConcreteString(text) => stringList(wordsOf(text))
          case _                    => stuck(listWordsFQN, s)
        }
    )

  private def wordsOf(text: String): Seq[String] = {
    val stripped = text.strip()
    if (stripped.isEmpty) Seq.empty else stripped.split(WhitespaceRun).toSeq
  }

  // ----------------------------------------------------------------------------------------------------------------
  // The chain
  // ----------------------------------------------------------------------------------------------------------------

  /** A concrete chain read as `(its empty node, its elements front to back)`. The `empty` node is kept as it stands so
    * a rebuilt chain ends in the same node (type argument included); a `prepend` cell is read from the *last two*
    * entries of its spine, so a cell built here (two entries) and one a stray unbound reference left with a written
    * type argument in front read the same. Anything else — a runtime parameter, a stuck `append`, a partial
    * application — is not a chain.
    */
  private object Chain {
    def unapply(v: SemValue): Option[(SemValue, List[SemValue])] = v match {
      case VTopDef(fqn, None, _, _) if fqn === listEmptyFQN       => Some((v, Nil))
      case VTopDef(fqn, None, spine, _) if fqn === listPrependFQN =>
        spine.toList match {
          case args if args.size >= 2 =>
            unapply(args(args.size - 2)).map { case (empty, rest) => (empty, args.last :: rest) }
          case _                      => None
        }
      case _                                                      => None
    }
  }

  /** The chain of `elements` (front to back) over `empty`. */
  private def chain(empty: SemValue, elements: Seq[SemValue]): SemValue =
    elements.foldRight(empty)((element, rest) => cell(rest, element))

  /** A chain of string literals over `empty[String]`. */
  private def stringList(pieces: Seq[String]): SemValue =
    chain(VTopDef(listEmptyFQN, None, Spine.SNil :+ stringType), pieces.map(stringValue))

  /** The canonical stuck form: a [[SemValue.VStuckNative]] carrying the FQN the residual call must name and the
    * not-yet-concrete value arguments, so it stays definitionally distinct and is re-fired once they concretise.
    */
  private def stuck(fqn: ValueFQN, args: SemValue*): SemValue = VStuckNative.of(fqn, args*)

  private object ConcreteString {
    def unapply(v: SemValue): Option[String] = v match {
      case VConst(GroundValue.Direct(Literal.StringValue(s), _)) => Some(s)
      case _                                                     => None
    }
  }

  private def stringValue(s: String): SemValue = VConst(GroundValue.Direct(s, Evaluator.stringGroundType))
}
