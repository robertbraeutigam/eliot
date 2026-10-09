package com.vanillasource.eliot.eliotc.jvm

/** The scope check's other direction (`docs/effects.md` §4): a definition may not declare an effect its body never
  * performs. A row says "my caller hands me an implementation of these", so an entry nothing consumes asks every caller
  * to declare an effect on no evidence.
  */
class OverDeclaredEffectIntegrationTest extends FullIntegrationTest {

  "a definition declaring an effect it never performs" should "be rejected at the entry" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def greet uses Console, Abort: Unit = printLine("hi")
        |
        |def main uses Console: Unit = greet else unit
        |""".stripMargin
    ).asserting(_ should include("declares the effect 'Abort' but does not perform it"))
  }

  // A slot whose entry the definition also declares rides the caller's binding, so the argument's effect is bound where
  // the argument is written and running it consumes nothing here — the same as a `{}` slot.
  "a pass-through of a computation at a slot naming the declared effect" should "be rejected" in {
    compileForErrors(
      """import eliot.effect.Console
        |
        |def passThrough[A](v uses *, Console: A) uses Console: A = v
        |
        |def main uses Console: Unit = passThrough(printLine("hi"))
        |""".stripMargin
    ).asserting(_ should include("declares the effect 'Console' but does not perform it"))
  }

  "an effect performed only through a declaring callee" should "count as performed" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |def shout(s: String) uses Console: Unit = printLine(s)
        |def greet uses Console: Unit = shout("hi")
        |
        |def main uses Console: Unit = greet
        |""".stripMargin
    ).asserting(_ shouldBe "hi")
  }

  "an effect performed only in one branch" should "count as performed" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def safe uses Abort: String = if(true, "granted")
        |
        |def main uses Console: Unit = printLine(safe else "denied")
        |""".stripMargin
    ).asserting(_ shouldBe "granted")
  }

  // An `implement` clause declares the union of its block's clause rows, so a clause is held only to what it wrote.
  private val terminalDouble =
    """import eliot.effect.Console
      |import eliot.effect.Writer
      |
      |effect Terminal {
      |   def say(s: String): Unit
      |   def ask: String
      |}
      |
      |implement fake: Terminal {
      |   def say(s: String) uses Writer[String]: Unit = tell(s)
      |   def ask: %s String = "nothing"
      |}
      |
      |def main uses Console: Unit = printLine(runWriterToLog({ say(ask) } with fake))
      |""".stripMargin

  "an implement clause declaring an effect it never performs" should "be rejected at that clause" in {
    compileForErrors(terminalDouble.format("{Writer[String]}"))
      .asserting(_ should include(":11:14:This value declares the effect 'Writer' but does not perform it"))
  }

  it should "not blame a clause for the effect a sibling clause declares" in {
    compileAndRun(terminalDouble.format("")).asserting(_ shouldBe "nothing")
  }

  // The usual cause of an unperformed declaration is a callee that performs the effect without declaring it. That
  // callee's error is the one to report, and the caller's declaration is not blamed for it.
  "a callee performing an effect it does not declare" should "be reported instead of its caller's declaration" in {
    compileForErrors(
      """import eliot.effect.Console
        |
        |def leak: Unit = printLine("hi")
        |
        |def main uses Console: Unit = leak
        |""".stripMargin
    ).asserting(_ should (include("performs the effect 'Console' but does not declare it") and not include "declares the effect 'Console' but does not perform it"))
  }
}
