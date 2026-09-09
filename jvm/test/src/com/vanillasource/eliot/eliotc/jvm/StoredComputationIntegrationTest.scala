package com.vanillasource.eliot.eliotc.jvm

/** A `data` field that **stores a computation** — effects v6 A7, `docs/effects.md` §9.5 "Storage".
  *
  * A row-typed field lowers to a thunk (`{Throw[E]} String` ⤳ `Unit -> String`), so storing one is ordinary data: the
  * effects are bound where the value is *constructed*, the field travels through plain generics, and running it is a
  * later, separate act. Three things have to line up for that to be true, and each was broken until A7:
  *
  *   - the value **constructor's** slot is a row position, so the actual delivered there is thunked and the entries it
  *     supplies are bound inside it. Thunking the field on the `data` erased the row before the split into functions
  *     could see it, and `Box(failing)` charged the effect to whoever built the value;
  *   - the **accessor** hands the field back as stored, which is *not* a return row: a return row would say the
  *     accessor performs those effects and would mint a phantom binder for them;
  *   - reading it **runs** it, so a saturated accessor call is applied — the mirror of a row-typed parameter
  *     reference, and what makes wrap and apply cancel at a rowed slot (`runThrow(step(t))`).
  *
  * The suite also stands on A6: `runThrow`'s `E` is determined from the stored field's own declared row, so none of
  * these programs writes a type argument.
  */
class StoredComputationIntegrationTest extends FullIntegrationTest {

  private val prelude =
    """import eliot.collection.List
      |import eliot.effect.Console
      |import eliot.effect.Throw
      |
      |data Task[E](step: {Throw[E]} String, label: String)
      |
      |def failing: {Throw[String]} String = raise("nope")
      |
      |""".stripMargin

  "a stored computation" should "run only when the field is read" in {
    compileAndRun(prelude + """
      |def held: Task[String] = Task(failing, "held")
      |def main: {Console} Unit = printLine(combine("built ", label(held)))""".stripMargin)
      .asserting(_ shouldBe "built held")
  }

  "reading a stored computation" should "perform its effect at the read" in {
    compileAndRun(prelude + """
      |def outcome(t: Task[String]): String = step(t) catch (e -> combine("caught ", e))
      |def main: {Console} Unit = printLine(outcome(Task(failing, "bad")))""".stripMargin)
      .asserting(_ shouldBe "caught nope")
  }

  "a pure value at a stored slot" should "be lifted into the thunk and read back unchanged" in {
    compileAndRun(prelude + """
      |def outcome(t: Task[String]): String = step(t) catch (e -> e)
      |def main: {Console} Unit = printLine(outcome(Task("plain", "ok")))""".stripMargin)
      .asserting(_ shouldBe "plain")
  }

  "a stored computation discharged with runThrow" should "materialise its outcome without a written type argument" in {
    compileAndRun(prelude + """
      |def outcome(t: Task[String]): String = foldEither(e -> e, v -> v, runThrow(step(t)))
      |def main: {Console} Unit = printLine(outcome(Task(failing, "bad")))""".stripMargin)
      .asserting(_ shouldBe "nope")
  }

  "stored computations in a list" should "travel through a plain generic and run one by one" in {
    compileAndRun(prelude + """
      |def tasks: List[Task[String]] =
      |   prepend(prepend(empty, Task("plain", "ok")), Task(failing, "bad"))
      |def outcome(t: Task[String]): String =
      |   combine(label(t), combine(": ", foldEither(e -> e, v -> v, runThrow(step(t)))))
      |def main: {Console} Unit = foreach(t -> printLine(outcome(t)), tasks)""".stripMargin)
      .asserting(_ shouldBe "bad: nope\nok: plain")
  }

  "a definition declaring the stored row" should "propagate it to its caller" in {
    compileAndRun(prelude + """
      |def propagate(t: Task[String]): {Throw[String]} String = step(t)
      |def main: {Console} Unit = printLine(propagate(Task(failing, "bad")) catch (e -> e))""".stripMargin)
      .asserting(_ shouldBe "nope")
  }

  "reading a stored computation without declaring its effect" should "be rejected at the read" in {
    compileForErrors(prelude + """
      |def leak(t: Task[String]): String = step(t)
      |def main: {Console} Unit = printLine(leak(Task(failing, "bad")))""".stripMargin)
      .asserting(_ should include("performs the effect 'Throw' but does not declare it"))
  }
}
