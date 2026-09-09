package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{mangleSuffix, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{javaInternalName, systemFunctionValue, systemUnitValue, types}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.{Label, Opcodes}

/** Effects v6's two control-flow primitives on the JVM (`docs/effects.md` §9.6) — the **escape** (a non-local exit)
  * and the **cell** (a value threaded through calls that never mention it). Neither is expressible as a def in a
  * strict pure core, which is the whole reason the carrier existed; they are the only two things left of it.
  *
  * Both come in a group of leaves sharing one frame, and both are **emitted once per instantiation** rather than once
  * erased. That is what makes `raise` leave through the frame of *its own* error type: an exit from a
  * `{Throw[NetError]}` passes straight through a `runThrow` installed for a `ParseError`, which a nearest-frame
  * discipline cannot do. The instantiation reaches the emitter as its ground type arguments, and the **first** of them
  * is the frame key by convention — every leaf here declares its key parameter first.
  *
  * Two things separate frames, at two scales. **Per effect**: each leaf is declared `private` in the module that needs
  * it (`eliot.effect.Throw` and `eliot.effect.Abort` each own an escape; `State`, `Writer` and `Dep` each own a cell),
  * so the fields below are per-module and an `Abort` escape can never see a `Throw` exit. **Per instantiation**: the
  * key suffix, so a `Throw[NetError]` frame can never see a `Throw[ParseError]` exit.
  *
  * The exit is a **pre-allocated shared exception plus a tag**, not a generated exception class per instantiation. It
  * is the same mechanism either way — the machine stack unwinds and the nearest *matching* handler wins, a
  * non-matching one rethrows — and it needs no class generation, which for a leaf that must interoperate with the
  * per-instantiation method naming is markedly less machinery. Reusing one instance also skips the stack-trace fill-in
  * that makes exceptions expensive: an exit carries no diagnostic, only a value.
  *
  * The leaves are written in **continuation-passing** form (`escapeInternal(body, onExit, onValue)`) so that no native
  * here constructs an Eliot `data`: the result `Either`/`Pair` is built by the ordinary Eliot body of the discharger
  * that calls it, and the bytecode only ever applies a `Function`. That is the same shape `foreverInternal` uses, and
  * it keeps the backend's knowledge of the standard library at zero.
  *
  * **Reference representations only.** A key or payload whose representation is a JVM primitive (`{State[Int]}`) is
  * rejected by [[unsupportedInstantiation]] rather than mis-emitted: the cell is an `Object` field and `Function.apply`
  * is `Object`-typed, so a primitive would need boxing decisions this does not make. The rejection is a loud build
  * error at the definition, never a silent wrong store.
  */
object ControlNatives {

  /** The leaves, keyed by FQN — the module part varies, so each is registered for every module that declares it. */
  val perInstantiation: Map[ValueFQN, (JvmIdentifier, Seq[GroundValue]) => NativeImplementation] =
    Map.from(
      escapeModules.flatMap(module =>
        Seq(
          effectValueFQN(module, "escapeInternal") -> escapeInternal,
          effectValueFQN(module, "exitInternal")   -> exitInternal
        )
      ) ++ cellModules.flatMap(module =>
        Seq(
          effectValueFQN(module, "withCellInternal")  -> withCellInternal,
          effectValueFQN(module, "readCellInternal")  -> readCellInternal,
          effectValueFQN(module, "writeCellInternal") -> writeCellInternal
        )
      )
    )

  /** The modules owning an escape group: the two abortive control effects. */
  private def escapeModules: Seq[String] = Seq("Throw", "Abort")

  /** The modules owning a cell group: the three stateful control effects. */
  private def cellModules: Seq[String] = Seq("State", "Writer", "Dep")

  /** The build error for an instantiation this cannot emit, or [[None]]. Read by [[JvmClassGenerator]] before it emits,
    * so the error lands at the definition rather than as invalid bytecode.
    */
  def unsupportedInstantiation(vfqn: ValueFQN, typeArgs: Seq[GroundValue]): Option[String] =
    typeArgs.find(isPrimitive).map { argument =>
      s"The control-flow primitive '${vfqn.name.name}' cannot be used at '${valueType(argument).name.name}', whose " +
        "machine representation is a primitive. Use a boxed type, or discharge the effect at a reference type."
    }

  private def isPrimitive(value: GroundValue): Boolean =
    types.get(valueType(value)).exists(_.javaClass.isPrimitive)

  /** The per-module fields. Names are prefixed so no Eliot value can collide with them. */
  private val exitSentinelField = JvmIdentifier("eliot$exit")
  private val exitTagField      = JvmIdentifier("eliot$exitTag")
  private val exitValueField    = JvmIdentifier("eliot$exitValue")

  private val throwableDescriptor = "Ljava/lang/RuntimeException;"
  private val stringDescriptor    = "Ljava/lang/String;"
  private val objectDescriptor    = "Ljava/lang/Object;"
  private val functionApply       = "(Ljava/lang/Object;)Ljava/lang/Object;"

  /** `escapeInternal[K, A, R](body: {} A, onExit: K => {} R, onValue: A => {} R): R` — install the frame keyed by `K`,
    * run `body` inside it, and hand the result to `onValue`, or whatever left through this frame to `onExit`.
    *
    * An exit that is not this frame's — a different key, or a genuine exception from the body — is rethrown, which is
    * exactly "the nearest *matching* enclosing frame": the stack keeps unwinding until one accepts.
    */
  private def escapeInternal(name: JvmIdentifier, typeArgs: Seq[GroundValue]): NativeImplementation =
    new NativeImplementation {
      override val impure: Boolean = true

      override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] =
        declareExitFields(classGenerator) >>
          classGenerator
            .createMethod[CompilerIO](
              name,
              Seq(systemFunctionValue, systemFunctionValue, systemFunctionValue),
              valueType(typeArgs(2))
            )
            .use { methodGenerator =>
              methodGenerator.runNative[CompilerIO] { methodVisitor =>
                val tryStart = new Label()
                val tryEnd   = new Label()
                val handler  = new Label()
                val rethrow  = new Label()
                val end      = new Label()

                methodVisitor.visitTryCatchBlock(tryStart, tryEnd, handler, "java/lang/RuntimeException")

                methodVisitor.visitLabel(tryStart)
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 2)                 // onValue
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)                 // body
                methodVisitor.visitInsn(Opcodes.ACONST_NULL)                 // its ignored Unit argument
                apply(methodVisitor)                                         // the body's value
                apply(methodVisitor)                                         // onValue(value)
                methodVisitor.visitLabel(tryEnd)
                methodVisitor.visitJumpInsn(Opcodes.GOTO, end)

                methodVisitor.visitLabel(handler)                            // [throwable]
                methodVisitor.visitInsn(Opcodes.DUP)
                methodVisitor.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  classGenerator.internalName,
                  exitSentinelField.value,
                  throwableDescriptor
                )
                methodVisitor.visitJumpInsn(Opcodes.IF_ACMPNE, rethrow)      // not an Eliot exit at all
                // The tag is the receiver's *constant*, so a never-set tag compares false instead of throwing.
                methodVisitor.visitLdcInsn(frameKey(typeArgs))
                methodVisitor.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  classGenerator.internalName,
                  exitTagField.value,
                  stringDescriptor
                )
                methodVisitor.visitMethodInsn(
                  Opcodes.INVOKEVIRTUAL,
                  "java/lang/String",
                  "equals",
                  "(Ljava/lang/Object;)Z",
                  false
                )
                methodVisitor.visitJumpInsn(Opcodes.IFEQ, rethrow)           // another instantiation's exit
                methodVisitor.visitInsn(Opcodes.POP)                         // drop the throwable
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)                 // onExit
                methodVisitor.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  classGenerator.internalName,
                  exitValueField.value,
                  objectDescriptor
                )
                apply(methodVisitor)                                         // onExit(value)
                methodVisitor.visitJumpInsn(Opcodes.GOTO, end)

                methodVisitor.visitLabel(rethrow)
                methodVisitor.visitInsn(Opcodes.ATHROW)

                methodVisitor.visitLabel(end)
              } >> methodGenerator.addCastTo[CompilerIO](valueType(typeArgs(2)))
            }
    }

  /** `exitInternal[K, A](err: K): A` — leave through the nearest enclosing frame of this key, carrying `err`. Control
    * does not return, so the `ARETURN` `createMethod` appends is dead code that `COMPUTE_FRAMES` rewrites.
    */
  private def exitInternal(name: JvmIdentifier, typeArgs: Seq[GroundValue]): NativeImplementation =
    new NativeImplementation {
      override val impure: Boolean = true

      override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] =
        declareExitFields(classGenerator) >>
          classGenerator
            .createMethod[CompilerIO](name, Seq(valueType(typeArgs.head)), valueType(typeArgs(1)))
            .use { methodGenerator =>
              methodGenerator.runNative[CompilerIO] { methodVisitor =>
                val allocated = new Label()

                // The sentinel is allocated on the first exit and reused: an exit carries a value, never a diagnostic,
                // so filling in a stack trace per raise would be pure cost. Unsynchronised, as `readLineInternal`'s
                // reader is, because the language cannot express a thread.
                methodVisitor.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  classGenerator.internalName,
                  exitSentinelField.value,
                  throwableDescriptor
                )
                methodVisitor.visitJumpInsn(Opcodes.IFNONNULL, allocated)
                methodVisitor.visitTypeInsn(Opcodes.NEW, "java/lang/RuntimeException")
                methodVisitor.visitInsn(Opcodes.DUP)
                methodVisitor.visitMethodInsn(
                  Opcodes.INVOKESPECIAL,
                  "java/lang/RuntimeException",
                  "<init>",
                  "()V",
                  false
                )
                methodVisitor.visitFieldInsn(
                  Opcodes.PUTSTATIC,
                  classGenerator.internalName,
                  exitSentinelField.value,
                  throwableDescriptor
                )
                methodVisitor.visitLabel(allocated)

                methodVisitor.visitLdcInsn(frameKey(typeArgs))
                methodVisitor.visitFieldInsn(
                  Opcodes.PUTSTATIC,
                  classGenerator.internalName,
                  exitTagField.value,
                  stringDescriptor
                )
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
                methodVisitor.visitFieldInsn(
                  Opcodes.PUTSTATIC,
                  classGenerator.internalName,
                  exitValueField.value,
                  objectDescriptor
                )
                methodVisitor.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  classGenerator.internalName,
                  exitSentinelField.value,
                  throwableDescriptor
                )
                methodVisitor.visitInsn(Opcodes.ATHROW)
              }
            }
    }

  /** `withCellInternal[S, A, R](initial: S, body: {} A, combine: A => S => {} R): R` — a cell holding an `S`, scoped to
    * this call: saved and restored around it, so a nested discharge of the same effect nests properly and an exit
    * leaving through it does not strand the outer value.
    */
  private def withCellInternal(name: JvmIdentifier, typeArgs: Seq[GroundValue]): NativeImplementation =
    new NativeImplementation {
      override val impure: Boolean = true

      override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
        val cell = cellField(typeArgs)

        classGenerator.createPrivateStaticFieldOnce[CompilerIO](cell, objectDescriptor) >>
          classGenerator
            .createMethod[CompilerIO](
              name,
              Seq(valueType(typeArgs.head), systemFunctionValue, systemFunctionValue),
              valueType(typeArgs(2))
            )
            .use { methodGenerator =>
              methodGenerator.runNative[CompilerIO] { methodVisitor =>
                val tryStart = new Label()
                val tryEnd   = new Label()
                val handler  = new Label()
                val end      = new Label()

                methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
                methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, classGenerator.internalName, cell.value, objectDescriptor)
                methodVisitor.visitVarInsn(Opcodes.ASTORE, 3)                // the enclosing value, to restore
                methodVisitor.visitFieldInsn(Opcodes.PUTSTATIC, classGenerator.internalName, cell.value, objectDescriptor)

                methodVisitor.visitTryCatchBlock(tryStart, tryEnd, handler, null)

                methodVisitor.visitLabel(tryStart)
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 2)                 // combine
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)                 // body
                methodVisitor.visitInsn(Opcodes.ACONST_NULL)
                apply(methodVisitor)                                         // the body's value
                apply(methodVisitor)                                         // combine(value)
                // Read *after* the body has run, so the final state is the one the body left.
                methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, classGenerator.internalName, cell.value, objectDescriptor)
                apply(methodVisitor)                                         // combine(value)(state)
                methodVisitor.visitVarInsn(Opcodes.ASTORE, 4)
                methodVisitor.visitLabel(tryEnd)
                restore(methodVisitor, classGenerator, cell)
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 4)
                methodVisitor.visitJumpInsn(Opcodes.GOTO, end)

                methodVisitor.visitLabel(handler)                            // [throwable]
                restore(methodVisitor, classGenerator, cell)
                methodVisitor.visitInsn(Opcodes.ATHROW)

                methodVisitor.visitLabel(end)
              } >> methodGenerator.addCastTo[CompilerIO](valueType(typeArgs(2)))
            }
      }

      private def restore(
          methodVisitor: org.objectweb.asm.MethodVisitor,
          classGenerator: ClassGenerator,
          cell: JvmIdentifier
      ): Unit = {
        methodVisitor.visitVarInsn(Opcodes.ALOAD, 3)
        methodVisitor.visitFieldInsn(Opcodes.PUTSTATIC, classGenerator.internalName, cell.value, objectDescriptor)
      }
    }

  /** `readCellInternal[S]: S` — the cell's current value. */
  private def readCellInternal(name: JvmIdentifier, typeArgs: Seq[GroundValue]): NativeImplementation =
    new NativeImplementation {
      override val impure: Boolean = true

      override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
        val cell = cellField(typeArgs)

        classGenerator.createPrivateStaticFieldOnce[CompilerIO](cell, objectDescriptor) >>
          classGenerator
            .createMethod[CompilerIO](name, Seq.empty, valueType(typeArgs.head))
            .use { methodGenerator =>
              methodGenerator.runNative[CompilerIO] { methodVisitor =>
                methodVisitor
                  .visitFieldInsn(Opcodes.GETSTATIC, classGenerator.internalName, cell.value, objectDescriptor)
              } >> methodGenerator.addCastTo[CompilerIO](valueType(typeArgs.head))
            }
      }
    }

  /** `writeCellInternal[S](s: S): Unit` — replace the cell's value. */
  private def writeCellInternal(name: JvmIdentifier, typeArgs: Seq[GroundValue]): NativeImplementation =
    new NativeImplementation {
      override val impure: Boolean = true

      override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
        val cell = cellField(typeArgs)

        classGenerator.createPrivateStaticFieldOnce[CompilerIO](cell, objectDescriptor) >>
          classGenerator
            .createMethod[CompilerIO](name, Seq(valueType(typeArgs.head)), systemUnitValue)
            .use { methodGenerator =>
              methodGenerator.runNative[CompilerIO] { methodVisitor =>
                methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
                methodVisitor
                  .visitFieldInsn(Opcodes.PUTSTATIC, classGenerator.internalName, cell.value, objectDescriptor)
                methodVisitor.visitInsn(Opcodes.ACONST_NULL)
              }
            }
      }
    }

  private def declareExitFields(classGenerator: ClassGenerator): CompilerIO[Unit] =
    classGenerator.createPrivateStaticFieldOnce[CompilerIO](exitSentinelField, throwableDescriptor) >>
      classGenerator.createPrivateStaticFieldOnce[CompilerIO](exitTagField, stringDescriptor) >>
      classGenerator.createPrivateStaticFieldOnce[CompilerIO](exitValueField, objectDescriptor)

  private def apply(methodVisitor: org.objectweb.asm.MethodVisitor): Unit =
    methodVisitor.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/function/Function", "apply", functionApply, true)

  /** The frame key of an instantiation: its **first** type argument, mangled exactly as a method-name suffix is, so a
    * key and the method names carrying it cannot disagree about what counts as the same instantiation.
    */
  private def frameKey(typeArgs: Seq[GroundValue]): String = mangleSuffix(typeArgs.take(1))

  private def cellField(typeArgs: Seq[GroundValue]): JvmIdentifier =
    JvmIdentifier.encode("eliot$cell" + frameKey(typeArgs))

  private def effectValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(ModuleName.effectPackage, moduleName), QualifiedName(valueName, Qualifier.Default))
}
