package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}

import java.nio.file.Path
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** End-to-end proof of the **result-edge re-encode** (`docs/string-length-meta.md` §8, stage S3) and of the first
  * *stated* meta transfer that needs it: `def length(s: String): Int {size(s)}`.
  *
  * A leaf's stated transfer pins a narrow range on the **call node** — `length("hello")` is `[5, 5]`, which the backend
  * lays out as a `java.lang.Byte`. But `String::length` is emitted as a generated native static method whose return
  * descriptor is the ⊤ bignum, so without a conversion on the result edge the class verifier rejects the caller
  * (`VerifyError: 'java/math/BigInteger' is not assignable to 'java/lang/Byte'`). The backend already widens *arguments*
  * to the bignum boundary; this is the mirror of that on the way out.
  *
  * The suite asserts both halves, because either alone would pass while the feature was broken: the program must
  * **run** (the verifier accepts the class), and the narrowing must actually be **emitted** (a backend that stamped
  * nothing at a call node would also run — and would throw the transfer away, which is option (2) §8 rejects).
  */
class StatedTransferResultEdgeIntegrationTest extends FullIntegrationTest {
  // The `docs/string-length-meta.md` §4.4 program: a literal's size flows through `length`'s stated transfer into the
  // `Int` domain, where an ordinary value-range `where` is discharged by it. Two domains, one `Interval` predicate.
  private val crossDomain =
    """|import eliot.jvm.IO
       |import eliot.effect.Console
       |def withinFive(i: Interval[BigInteger]): Bool = rangeWithin[0, 5](i)
       |def small(x: Int): Int where withinFive(range(x)) = x
       |""".stripMargin

  "a call whose stated transfer narrows its result" should "run rather than fail class verification" in {
    compileAndRun(crossDomain + """def main: IO[Unit] = printLine(show(small(length("hello"))))""")
      .asserting(_ shouldBe "5")
  }

  // `[5, 5]` lays out as a `java.lang.Byte`, so the bignum the native hands back is unboxed, narrowed and reboxed —
  // the same conversion `generateArgumentToBignum` performs in the other direction on the way into a call.
  it should "emit the narrowing conversion on the call's result edge" in {
    compileAndRun(crossDomain + """def main: IO[Unit] = printLine(show(small(length("hello"))))""") >>
      instructionsFollowing("eliot/lang/String.length", 4).asserting(
        _ shouldBe Seq("INVOKEVIRTUAL java/math/BigInteger.longValue", "L2I", "I2B", "INVOKESTATIC java/lang/Byte.valueOf")
      )
  }

  // The size a literal seeds is carried into the `Int` domain exactly, not merely widened into it: an eleven-code-point
  // literal fails the same precondition the five-code-point one discharges. Without the transfer this call is ⊤ and the
  // precondition can only be *unprovable*, never *violated*.
  it should "carry the literal's size into the Int domain precisely enough to reject" in {
    compileForErrors(crossDomain + """def main: IO[Unit] = printLine(show(small(length("hello world"))))""")
      .asserting(_ should include("precondition of 'Test::small' is not satisfied"))
  }

  // The re-encode is derived from the node's own meta, so a result the channel could not pin stays at the bignum
  // boundary and nothing is emitted at all — the ⊤ case every call without a stated transfer takes. `twice` is bodied,
  // so by R3 it states nothing and derives nothing; its result goes straight on to `show`.
  "a call with no stated transfer" should "leave its result at the boundary width" in {
    compileAndRun(
      """|import eliot.jvm.IO
         |import eliot.effect.Console
         |def twice(x: Int): Int = x + x
         |def main: IO[Unit] = printLine(show(twice(3)))""".stripMargin
    ) >> instructionsFollowing("Test.twice", 1).asserting(_ shouldBe Seq("INVOKEVIRTUAL java/math/BigInteger.toString"))
  }

  /** The `count` instructions the compiled jar emits immediately after its (first) call to `owner.name`, rendered as
    * the same opcode/target tokens `javap -c` shows. A program that does not call it at all answers with a sentinel
    * rather than an empty window, so "the call vanished" can never pass as "the call emitted no conversion".
    */
  private def instructionsFollowing(call: String, count: Int): IO[Seq[String]] = IO.blocking {
    val emitted = classesOf(jarPath).flatMap(instructionsOf)
    emitted.indexOf(s"INVOKESTATIC $call") match {
      case -1    => Seq(s"$call is never called")
      case index => emitted.slice(index + 1, index + 1 + count)
    }
  }

  private def classesOf(jar: Path): Seq[Array[Byte]] =
    Using.resource(new JarFile(jar.toFile)) { jarFile =>
      jarFile
        .entries()
        .asScala
        .toSeq
        .filter(_.getName.endsWith(".class"))
        .sortBy(_.getName)
        .map(entry => jarFile.getInputStream(entry).readAllBytes())
    }

  /** Every call and integer-conversion instruction of every method of one class, in emission order. Only these two
    * kinds are collected: they are exactly what a representation conversion consists of, so an unrelated instruction
    * between two of them cannot make the assertion above pass or fail spuriously.
    */
  private def instructionsOf(classBytes: Array[Byte]): Seq[String] = {
    val collected = Seq.newBuilder[String]
    new ClassReader(classBytes).accept(
      new ClassVisitor(Opcodes.ASM9) {
        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): MethodVisitor =
          new MethodVisitor(Opcodes.ASM9) {
            override def visitMethodInsn(
                opcode: Int,
                owner: String,
                methodName: String,
                methodDescriptor: String,
                isInterface: Boolean
            ): Unit = collected += s"${opcodeName(opcode)} $owner.$methodName"

            override def visitInsn(opcode: Int): Unit = opcode match {
              case Opcodes.L2I => collected += "L2I"
              case Opcodes.I2B => collected += "I2B"
              case Opcodes.I2S => collected += "I2S"
              case _           => ()
            }
          }
      },
      ClassReader.SKIP_FRAMES
    )
    collected.result()
  }

  private def opcodeName(opcode: Int): String = opcode match {
    case Opcodes.INVOKESTATIC    => "INVOKESTATIC"
    case Opcodes.INVOKEVIRTUAL   => "INVOKEVIRTUAL"
    case Opcodes.INVOKEINTERFACE => "INVOKEINTERFACE"
    case Opcodes.INVOKESPECIAL   => "INVOKESPECIAL"
    case _                       => "INVOKEDYNAMIC"
  }
}
