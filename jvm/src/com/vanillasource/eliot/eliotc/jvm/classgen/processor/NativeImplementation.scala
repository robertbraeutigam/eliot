package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemLangType
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.Opcodes

trait NativeImplementation {

  /** Whether this native touches the world (I/O) and therefore must be wrapped behind a `private` Eliot leaf. The
    * backend asserts the resolved def of an `impure` native is `Visibility.Private` (see [[JvmClassGenerator]]); a pure
    * native (arithmetic, `unit`) stays `false` and may be `public`.
    */
  def impure: Boolean = false

  def generateMethod(mainClassGenerator: ClassGenerator): CompilerIO[Unit]
}

object NativeImplementation {
  val implementations: Map[ValueFQN, NativeImplementation] = Map.from(
    Seq(
      (systemLangValueFQN("Console", "printlnInternal"), eliot_lang_Console_printlnInternal),
      (systemLangValueFQN("Console", "readLineInternal"), eliot_lang_Console_readLineInternal),
      (systemLangValueFQN("Unit", "unit"), eliot_lang_Unit_unit)
    )
  )

  private def systemLangValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, moduleName), QualifiedName(valueName, Qualifier.Default))

  /** The fail-safe boundary predicate: an `impure` native must be declared `private`. Returns the build-error message
    * when that invariant is violated (an impure native registered against a non-`private` def), `None` otherwise. Pure
    * natives are unconstrained. Kept pure (no fact access) so the enforcement decision is unit-testable directly.
    */
  def visibilityViolation(vfqn: ValueFQN, impure: Boolean, visibility: Visibility): Option[String] =
    Option.when(impure && visibility != Visibility.Private)(
      s"Impure native '${vfqn.show}' must be declared `private`."
    )

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

  private def eliot_lang_Console_printlnInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("printlnInternal"), Seq(systemLangType("String")), systemLangType("Unit"))
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
}
