package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The compile-time twins of the `eliot.collection.List` leaves, fired directly as the evaluator fires them: a concrete
  * list is a `prepend` chain over `empty`, `append`/`foldLeftInternal` reduce over it and stay stuck otherwise, and
  * `split`/`words` build one from a literal string with exactly the pieces the JVM leaves answer.
  */
class ListReductionsTest extends AnyFlatSpec with Matchers {
  private def native(fqn: ValueFQN): SemValue = ListReductions.bindings(fqn)

  private def app(f: SemValue, args: SemValue*): SemValue = args.foldLeft(f)(Evaluator.applyValue)

  private def spineOf(args: SemValue*): Spine = args.foldLeft(Spine.SNil: Spine)(_ :+ _)

  /** A rigid bound variable — a runtime parameter, the one thing a list built from it can never reduce over. */
  private def rigid(name: String): SemValue = VNeutral(NeutralHead.Param(0, name), Spine.SNil)

  private def str(s: String): SemValue = VConst(GroundValue.Direct(s, Evaluator.stringGroundType))

  private val stringType: SemValue = VTopDef(stringFQN, None, Spine.SNil)
  private val empty: SemValue      = VTopDef(listEmptyFQN, None, Spine.SNil)
  private val emptyOfString: SemValue = VTopDef(listEmptyFQN, None, spineOf(stringType))

  private def cell(list: SemValue, element: SemValue): SemValue = VTopDef(listPrependFQN, None, spineOf(list, element))

  private def chain(empty: SemValue, elements: SemValue*): SemValue = elements.foldRight(empty)((e, rest) => cell(rest, e))

  private def strings(pieces: String*): SemValue = chain(emptyOfString, pieces.map(str)*)

  /** A combining function that records its applications: `e -> acc -> mark(acc, e)`. */
  private val markFqn: ValueFQN =
    ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName("mark", Qualifier.Default))
  private def mark(acc: SemValue, e: SemValue): SemValue = VTopDef(markFqn, None, spineOf(acc, e))
  private val combine: SemValue = VLam("e", e => VLam("acc", acc => mark(acc, e)))

  private val sa = str("a")
  private val sb = str("b")
  private val sz = str("z")

  "empty" should "be its own constructor application" in {
    native(listEmptyFQN) shouldBe empty
  }

  "prepend" should "reduce to the constructor cell of the chain" in {
    app(native(listPrependFQN), empty, sa) shouldBe cell(empty, sa)
  }

  "append" should "put the element at the back of the chain" in {
    app(native(listAppendFQN), cell(empty, sa), sb) shouldBe chain(empty, sa, sb)
  }

  it should "start a chain from the same empty node it was handed" in {
    app(native(listAppendFQN), emptyOfString, sa) shouldBe cell(emptyOfString, sa)
  }

  it should "accept an element that is not concrete" in {
    app(native(listAppendFQN), empty, rigid("x")) shouldBe cell(empty, rigid("x"))
  }

  it should "stay stuck on a list that is not a chain" in {
    app(native(listAppendFQN), rigid("xs"), sb) shouldBe VStuckNative(listAppendFQN, spineOf(rigid("xs"), sb))
  }

  "foldLeftInternal" should "combine the elements front to back from the initial value" in {
    app(native(listFoldLeftInternalFQN), chain(empty, sa, sb), sz, combine) shouldBe mark(mark(sz, sa), sb)
  }

  it should "answer the initial value over the empty chain" in {
    app(native(listFoldLeftInternalFQN), empty, sz, combine) shouldBe sz
  }

  it should "read a cell carrying a written type argument in front of its two values" in {
    val withTypeArg = VTopDef(listPrependFQN, None, spineOf(stringType, empty, sa))
    app(native(listFoldLeftInternalFQN), withTypeArg, sz, combine) shouldBe mark(sz, sa)
  }

  it should "stay stuck on a list that is not a chain" in {
    app(native(listFoldLeftInternalFQN), rigid("xs"), sz, combine) shouldBe
      VStuckNative(listFoldLeftInternalFQN, spineOf(rigid("xs"), sz, combine))
  }

  "split" should "keep every empty piece, the trailing one included" in {
    app(native(listSplitFQN), str(","), str("a,b,")) shouldBe strings("a", "b", "")
  }

  it should "take the separator literally" in {
    app(native(listSplitFQN), str("."), str("a.b")) shouldBe strings("a", "b")
  }

  it should "cut at code points for the empty separator" in {
    app(native(listSplitFQN), str(""), str("a😀b")) shouldBe strings("a", "😀", "b")
  }

  it should "answer the empty list for the empty string and the empty separator" in {
    app(native(listSplitFQN), str(""), str("")) shouldBe emptyOfString
  }

  it should "stay stuck on a string that is not a literal" in {
    app(native(listSplitFQN), str(","), rigid("s")) shouldBe VStuckNative(listSplitFQN, spineOf(str(","), rigid("s")))
  }

  "words" should "answer the non-empty whitespace-separated pieces" in {
    app(native(listWordsFQN), str("  a \t b\n")) shouldBe strings("a", "b")
  }

  it should "answer the empty list for a blank string" in {
    app(native(listWordsFQN), str(" \t ")) shouldBe emptyOfString
  }

  it should "stay stuck on a string that is not a literal" in {
    app(native(listWordsFQN), rigid("s")) shouldBe VStuckNative(listWordsFQN, spineOf(rigid("s")))
  }
}
