package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemCollectionType, systemLangType}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemAnyValue, systemFunctionValue, systemUnitValue}
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
      (systemEffectValueFQN("Console", "isNull"), eliot_lang_Console_isNull),
      (systemEffectValueFQN("Log", "logInternal"), eliot_lang_Log_logInternal),
      (systemEffectValueFQN("Inf", "foreverInternal"), eliot_lang_Inf_foreverInternal),
      (systemLangValueFQN("Unit", "unit"), eliot_lang_Unit_unit),
      (collectionValueFQN("List", "empty"), eliot_collection_List_empty),
      (collectionValueFQN("List", "append"), eliot_collection_List_append),
      (collectionValueFQN("List", "foldLeftInternal"), eliot_collection_List_foldLeftInternal)
    ) ++ StringNatives.implementations ++ FileNatives.implementations
  )

  /** The erased JVM signature (all type-parameter positions collapsed to `Any`/`Object`, `List[A]` to `java.util.List`)
    * a *generic* native is emitted with and, crucially, *called through*. A generic operation is monomorphized per
    * element type by the front-end, so its call sites would otherwise link to per-instantiation mangled methods
    * (`append$Int`, `append$String`); instead these ops are emitted once, erased, and every call site resolves to that
    * single method by the plain name + this signature (see [[ExpressionCodeGenerator]]). One source of truth: the same
    * signature builds the method (definition) and the `INVOKESTATIC` descriptor (call).
    */
  case class GenericNativeSignature(parameterTypes: Seq[ValueFQN], returnType: ValueFQN)

  private val listType: ValueFQN = systemCollectionType("List")

  val genericNativeSignatures: Map[ValueFQN, GenericNativeSignature] = Map(
    collectionValueFQN("List", "empty")     -> GenericNativeSignature(Seq.empty, listType),
    collectionValueFQN("List", "append")    -> GenericNativeSignature(Seq(listType, systemAnyValue), listType),
    collectionValueFQN("List", "foldLeftInternal") ->
      GenericNativeSignature(Seq(listType, systemAnyValue, systemFunctionValue), systemAnyValue),
    systemEffectValueFQN("Console", "isNull") -> GenericNativeSignature(Seq(systemAnyValue), systemLangType("Bool"))
  ) ++ FileNatives.genericNativeSignatures

  private def collectionValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "collection"), moduleName), QualifiedName(valueName, Qualifier.Default))

  /** Natives attached *directly* to an ability-implementation method, rather than to a plain `Default`-qualified leaf
    * keyed in [[implementations]]. An impl method's FQN carries a per-module index assigned during resolution, so it
    * cannot be keyed statically; it is recognised by `(ability, method, dispatch type)` through the impl marker
    * ([[ImplementationMarkerUtils.isImplementationMethodFor]], done in [[JvmClassGenerator]]). Because the emitted method
    * must carry the impl method's *mangled* name (not the native's own local name), each maker takes that name and
    * produces the native for it — the value-level counterpart of `StdlibNativesProcessor`'s compile-time `Eq[String]`
    * native. The `String` instances (`Eq`/`Compare`/`Combine`, realised as `String.equals`/`.compareTo`/`.concat`) live
    * with the rest of the string leaves in [[StringNatives]]; further runtime ability leaves are added here or in the
    * per-domain object they belong to.
    */
  val abilityImplementations: Seq[(String, String, String, JvmIdentifier => NativeImplementation)] =
    StringNatives.abilityImplementations ++ FileNatives.abilityImplementations

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

  /** `empty[A]: List[A]` — a fresh empty list. A new `ArrayList` (mutated only by `append`, which always copies, so
    * every list Eliot hands out is used immutably).
    */
  private def eliot_collection_List_empty: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      val sig = genericNativeSignatures(collectionValueFQN("List", "empty"))
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("empty"), sig.parameterTypes, sig.returnType)
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList")
            methodVisitor.visitInsn(Opcodes.DUP)
            methodVisitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false)
          }
        }
    }
  }

  /** `append[A](list: List[A], element: A): List[A]` — a new list = `list` with `element` at the end. Copies `list`
    * into a fresh `ArrayList` (so the input is never mutated — value semantics), appends, and returns it. O(n) per call;
    * the interim construction cost is accepted until a uniqueness optimization can build in place.
    */
  private def eliot_collection_List_append: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      val sig = genericNativeSignatures(collectionValueFQN("List", "append"))
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("append"), sig.parameterTypes, sig.returnType)
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList")
            methodVisitor.visitInsn(Opcodes.DUP)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)                                // the source list
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESPECIAL,
              "java/util/ArrayList",
              "<init>",
              "(Ljava/util/Collection;)V",
              false
            )
            methodVisitor.visitInsn(Opcodes.DUP)                                        // keep the copy to return
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)                                // the element
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "java/util/ArrayList",
              "add",
              "(Ljava/lang/Object;)Z",
              false
            )
            methodVisitor.visitInsn(Opcodes.POP)                                        // discard the boolean
          }
        }
    }
  }

  /** `foldLeftInternal[A, B](list: List[A], initial: B, combine: A -> B -> B): B` — a left fold, front to back:
    * `acc = initial; for (e in list) acc = combine(e)(acc)`. The list-first primitive behind the dot-chainable
    * `List.foldLeft` wrapper (which reorders to `foldLeftInternal(list, initial, combine)`); kept list-first here so it
    * is always called fully applied, never partially. The curried `combine` is a `java.util.function.Function`
    * returning a `Function`. Only touches JDK interfaces (`List`/`Iterator`/`Function`), so the erased body serves every
    * instantiation.
    */
  private def eliot_collection_List_foldLeftInternal: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      val sig = genericNativeSignatures(collectionValueFQN("List", "foldLeftInternal"))
      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("foldLeftInternal"), sig.parameterTypes, sig.returnType)
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            val loop = new Label()
            val done = new Label()
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)                                // initial
            methodVisitor.visitVarInsn(Opcodes.ASTORE, 3)                               // acc = initial
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)                                // list
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEINTERFACE,
              "java/util/List",
              "iterator",
              "()Ljava/util/Iterator;",
              true
            )
            methodVisitor.visitVarInsn(Opcodes.ASTORE, 4)                               // it = list.iterator()
            methodVisitor.visitLabel(loop)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 4)
            methodVisitor.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true)
            methodVisitor.visitJumpInsn(Opcodes.IFEQ, done)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 2)                                // combine
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 4)
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEINTERFACE,
              "java/util/Iterator",
              "next",
              "()Ljava/lang/Object;",
              true
            )
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEINTERFACE,
              "java/util/function/Function",
              "apply",
              "(Ljava/lang/Object;)Ljava/lang/Object;",
              true
            )                                                                          // combine.apply(element)
            methodVisitor.visitTypeInsn(Opcodes.CHECKCAST, "java/util/function/Function")
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 3)                                // acc
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKEINTERFACE,
              "java/util/function/Function",
              "apply",
              "(Ljava/lang/Object;)Ljava/lang/Object;",
              true
            )                                                                          // .apply(acc)
            methodVisitor.visitVarInsn(Opcodes.ASTORE, 3)                               // acc = newAcc
            methodVisitor.visitJumpInsn(Opcodes.GOTO, loop)
            methodVisitor.visitLabel(done)
            methodVisitor.visitVarInsn(Opcodes.ALOAD, 3)                                // return acc
          }
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

  /** The name of the private static field holding the process-wide standard-input reader. See
    * [[eliot_lang_Console_readLineInternal]] for why it has to be a field.
    */
  private val stdinReaderField = JvmIdentifier("stdinReader")

  private def eliot_lang_Console_readLineInternal: NativeImplementation = new NativeImplementation {
    override val impure: Boolean = true

    /** Reads one line from standard input through a **single shared** `BufferedReader`, created on first use and kept
      * in a private static field.
      *
      * The reader has to be shared, and this is not an optimisation. A `BufferedReader` reads *ahead*: the first
      * `readLine` pulls everything currently available on the stream into its buffer and hands back only the first
      * line. Constructing a fresh reader per call — which this native used to do — therefore threw the rest of the
      * input away with the discarded reader, so the second `readLine` saw end-of-stream and returned `null`, and the
      * first `String` operation on that `null` failed inside a native with no Eliot-level explanation. A program could
      * read exactly one line, whatever it asked for.
      *
      * Initialised **lazily, on the first read** rather than in a static initialiser: the field is created the moment
      * it is first needed, which keeps `System.in` captured as late as possible. That matters because this class also
      * hosts `printLineInternal`, so class initialisation happens on the first *print* — potentially before a host that
      * embeds the generated jar in its own JVM has redirected standard input (which is exactly what the integration
      * harness does). The lazy check is unsynchronised: Eliot has no concurrency to race with it — the language cannot
      * express a thread any more than it can express recursion. Should that ever change, the fix is the holder-class
      * idiom (a nested class whose own initialiser builds the reader), not a lock here.
      */
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      val readerDescriptor = "Ljava/io/BufferedReader;"

      classGenerator.createPrivateStaticField[CompilerIO](stdinReaderField, readerDescriptor) >>
        classGenerator
          .createMethod[CompilerIO](JvmIdentifier("readLineInternal"), Seq.empty, systemLangType("String"))
          .use { methodGenerator =>
            methodGenerator.runNative { methodVisitor =>
              // if (stdinReader == null) stdinReader = new BufferedReader(new InputStreamReader(System.in));
              // return stdinReader.readLine();  — the ARETURN is appended by createMethod, so the line String is just
              // left on the stack.
              val initialised = new Label()

              methodVisitor.visitFieldInsn(
                Opcodes.GETSTATIC,
                classGenerator.internalName,
                stdinReaderField.value,
                readerDescriptor
              )
              methodVisitor.visitJumpInsn(Opcodes.IFNONNULL, initialised)

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
              methodVisitor.visitFieldInsn(
                Opcodes.PUTSTATIC,
                classGenerator.internalName,
                stdinReaderField.value,
                readerDescriptor
              )

              methodVisitor.visitLabel(initialised)
              methodVisitor.visitFieldInsn(
                Opcodes.GETSTATIC,
                classGenerator.internalName,
                stdinReaderField.value,
                readerDescriptor
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

  /** `true` when the argument is the null reference. Pure, and generic (erased to `Object`) like its `eliot.file`
    * namesake, whose bytecode this repeats — the two are separate registrations because per-file name resolution makes
    * each module declare its own leaf, and `eliot.effect.Console` must not depend on `eliot.file`.
    *
    * The single consumer is `Console.lineOrNone`, which turns the `null` that `java.io.BufferedReader.readLine` returns
    * at end of input into a `None`. That null is the one value crossing into Eliot that no Eliot type admits, so it is
    * confined to the two lines between this leaf and the `Option` the ability actually yields.
    */
  private def eliot_lang_Console_isNull: NativeImplementation = new NativeImplementation {

    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      val signature = genericNativeSignatures(systemEffectValueFQN("Console", "isNull"))

      classGenerator
        .createMethod[CompilerIO](JvmIdentifier("isNull"), signature.parameterTypes, signature.returnType)
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            val wasNull = new Label()
            val end     = new Label()

            methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
            methodVisitor.visitJumpInsn(Opcodes.IFNULL, wasNull)
            methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/Boolean", "FALSE", "Ljava/lang/Boolean;")
            methodVisitor.visitJumpInsn(Opcodes.GOTO, end)
            methodVisitor.visitLabel(wasNull)
            methodVisitor.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/Boolean", "TRUE", "Ljava/lang/Boolean;")
            methodVisitor.visitLabel(end)
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
