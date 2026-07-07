package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemLangType
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.{Label, Opcodes}

trait NativeImplementation {

  /** Whether this native must be wrapped behind a `private` Eliot leaf — because it touches the world (I/O, e.g.
    * `printLineInternal`) or is the one unbounded-loop divergence leaf (`foreverInternal`, the `Inf` effect). Either way
    * application code must never name it directly: the effect (I/O via `{Console}`/`{Log}`, divergence via `{Inf}`) is
    * recorded honestly only through the public ability the leaf hides behind. The backend asserts the resolved def of
    * an `impure` native is `Visibility.Private` (see [[JvmClassGenerator]]); a pure, total native (arithmetic, `unit`)
    * stays `false` and may be `public`.
    */
  def impure: Boolean = false

  def generateMethod(mainClassGenerator: ClassGenerator): CompilerIO[Unit]
}

object NativeImplementation {
  val implementations: Map[ValueFQN, NativeImplementation] = Map.from(
    Seq(
      (systemEffectValueFQN("Console", "printLineInternal"), eliot_lang_Console_printLineInternal),
      (systemEffectValueFQN("Console", "readLineInternal"), eliot_lang_Console_readLineInternal),
      (systemEffectValueFQN("Log", "logInternal"), eliot_lang_Log_logInternal),
      (systemEffectValueFQN("Inf", "foreverInternal"), eliot_lang_Inf_foreverInternal),
      (systemLangValueFQN("Unit", "unit"), eliot_lang_Unit_unit),
      (systemLangValueFQN("Eq", "stringEquals"), eliot_lang_Eq_stringEquals)
    )
  )

  private def systemLangValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, moduleName), QualifiedName(valueName, Qualifier.Default))

  /** The effect-package counterpart of [[systemLangValueFQN]]: the `Console`/`Log`/`Inf` native leaves live in
    * `eliot.effect` (see [[ModuleName.effectPackage]]), while `Unit.unit` stays in `eliot.lang`.
    */
  private def systemEffectValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(ModuleName.effectPackage, moduleName), QualifiedName(valueName, Qualifier.Default))

  /** The fail-safe boundary predicate: an `impure` native must be declared `private`. Returns the build-error message
    * when that invariant is violated (an impure native registered against a non-`private` def), `None` otherwise. Pure
    * natives are unconstrained. Kept pure (no fact access) so the enforcement decision is unit-testable directly.
    */
  def visibilityViolation(vfqn: ValueFQN, impure: Boolean, visibility: Visibility): Option[String] =
    Option.when(impure && visibility != Visibility.Private)(
      s"Impure native '${vfqn.show}' must be declared `private`."
    )

  /** `forever` (the `Inf` effect, termination M1): run a deferred thunk (`Function[Unit, Unit]`) endlessly — a plain
    * `while (true) { thunk.apply(unit) }`. It is the single trusted source of divergence: a body-less native is
    * terminating by default, and this is the one that loops. The method never returns normally, so it has no reachable
    * return; the trailing `ARETURN` `createMethod` appends is dead code, which `COMPUTE_FRAMES` rewrites. Marked
    * `impure` so the backend enforces its `private` declaration (divergence is reachable only through `{Inf}` `forever`).
    */
  private def eliot_lang_Inf_foreverInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] =
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("foreverInternal"), Seq(systemFunctionValue), systemUnitValue)
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            val loop = new Label()
            methodVisitor.visitLabel(loop)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)  // the thunk
            methodVisitor.visitInsn(Opcodes.ACONST_NULL)  // the Unit argument
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEINTERFACE,
              "java/util/function/Function",
              "apply",
              "(Ljava/lang/Object;)Ljava/lang/Object;",
              true
            )
            methodVisitor.visitInsn(Opcodes.POP)          // discard the Unit result
            methodVisitor.visitJumpInsn(Opcodes.GOTO, loop)
          }
        }
  }

  /** `stringEquals(a: String, b: String): Bool` — the value-equality leaf behind the runtime `Eq[String]` instance
    * (`jvm/.../Eq.els`). Realised as `Boolean.valueOf(a.equals(b))`, so the opaque `Bool` result is the boxed
    * `java.lang.Boolean` the backend carries `Bool` as (see [[NativeType]]). Pure (`impure = false`), so it may stay a
    * plain — here `private` — leaf. Its compile-time counterpart is `StdlibNativesProcessor.stringEquals`.
    */
  private def eliot_lang_Eq_stringEquals: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] =
      classGenerator
        .createMethod[CompilerIO](
          JvmIdentifier("stringEquals"),
          Seq(systemLangType("String"), systemLangType("String")),
          systemLangType("Bool")
        )
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/lang/String",
              "equals",
              "(Ljava/lang/Object;)Z",
              false
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESTATIC,
              "java/lang/Boolean",
              "valueOf",
              "(Z)Ljava/lang/Boolean;",
              false
            )
          }
        }
  }

  private def eliot_lang_Unit_unit: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("unit"), Seq.empty, systemLangType("Unit"))
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitInsn(Opcodes.ACONST_NULL)
          }
        }
    }
  }

  private def eliot_lang_Console_printLineInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("printLineInternal"), Seq(systemLangType("String")), systemLangType("Unit"))
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/io/PrintStream",
              "println",
              "(Ljava/lang/String;)V",
              false
            )
            methodVisitor.visitInsn(Opcodes.ACONST_NULL)
          }
        }
    }
  }

  private def eliot_lang_Console_readLineInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("readLineInternal"), Seq.empty, systemLangType("String"))
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            // new BufferedReader(new InputStreamReader(System.in)).readLine() — leaves the line String on the stack;
            // createMethod appends the ARETURN. Reads one line per call (sufficient for the current Console effect).
            methodVisitor.visitTypeInsn(Opcodes.NEW, "java/io/BufferedReader")
            methodVisitor.visitInsn(Opcodes.DUP)
            methodVisitor.visitTypeInsn(Opcodes.NEW, "java/io/InputStreamReader")
            methodVisitor.visitInsn(Opcodes.DUP)
            methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESPECIAL,
              "java/io/InputStreamReader",
              "<init>",
              "(Ljava/io/InputStream;)V",
              false
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESPECIAL,
              "java/io/BufferedReader",
              "<init>",
              "(Ljava/io/Reader;)V",
              false
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/io/BufferedReader",
              "readLine",
              "()Ljava/lang/String;",
              false
            )
          }
        }
    }
  }

  private def eliot_lang_Log_logInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("logInternal"), Seq(systemLangType("String")), systemLangType("Unit"))
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            // System.out.println("[LOG] " + s) — emit a tagged diagnostic line to standard output (captured by the
            // integration tests, and visually distinct from a plain Console `printLine`).
            methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            methodVisitor.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder")
            methodVisitor.visitInsn(Opcodes.DUP)
            methodVisitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false)
            methodVisitor.visitLdcInsn("[LOG] ")
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/lang/StringBuilder",
              "append",
              "(Ljava/lang/String;)Ljava/lang/StringBuilder;",
              false
            )
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/lang/StringBuilder",
              "append",
              "(Ljava/lang/String;)Ljava/lang/StringBuilder;",
              false
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/lang/StringBuilder",
              "toString",
              "()Ljava/lang/String;",
              false
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/io/PrintStream",
              "println",
              "(Ljava/lang/String;)V",
              false
            )
            methodVisitor.visitInsn(Opcodes.ACONST_NULL)
          }
        }
    }
  }
}
