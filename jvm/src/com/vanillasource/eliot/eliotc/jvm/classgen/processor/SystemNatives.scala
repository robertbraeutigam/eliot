package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemAnyValue, systemCollectionType, systemLangType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.GenericNativeSignature
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.{Label, MethodVisitor}

/** The JVM leaf natives behind `eliot.system` — the process environment (`Environment`) and running other programs
  * (`Process`).
  *
  * Same two constraints as [[FileNatives]], for the same reasons: a native never constructs an Eliot `data` value in
  * bytecode, so `Environment.environmentVariable` returns a nullable that Eliot turns into an `Option`, and the process
  * operations catch their exception into a four-slot `Object[]` holder (`ProcessOutcome`) that the Eliot instance
  * reflects into `Throw[IoError]`.
  *
  * The one piece that is not a plain delegation is `argumentsInternal`. A JVM program receives its arguments as the
  * parameter of `main`, which is a *stack slot of a method in another class* — nothing a leaf native can reach. So
  * [[JvmClassGenerator]] has the synthesized entry point stash them in a public static field of its own class
  * ([[SystemNatives.argumentsHolderClass]] / [[SystemNatives.argumentsField]]) before calling the user's `main`, and
  * this native reads that field. The entry-point class is the one class an executable jar is guaranteed to have, which
  * is why the field lives there rather than on `Environment`'s own class — a program that never mentions `Environment`
  * does not generate that class at all.
  */
object SystemNatives {

  private def environmentFqn(name: String): ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "system"), "Environment"), QualifiedName(name, Qualifier.Default))
  private def processFqn(name: String): ValueFQN     =
    ValueFQN(ModuleName(Seq("eliot", "system"), "Process"), QualifiedName(name, Qualifier.Default))

  private val pathType: ValueFQN    = NativeType.systemFileType("Path", "Path")
  private val outcomeType: ValueFQN = NativeType.systemSystemType("Process", "ProcessOutcome")
  private val stringType: ValueFQN  = systemLangType("String")
  private val boolType: ValueFQN    = systemLangType("Bool")
  private val intType: ValueFQN     = systemLangType("Int")
  private val listType: ValueFQN    = systemCollectionType("List")

  /** The class the synthesized entry point stashes the program arguments in — module `main`, the executable jar's
    * `Main-Class`, whose internal name is just `main` (see `SyntheticMainSourceProcessor`).
    */
  val argumentsHolderClass: String = "main"

  /** The field the arguments are stashed in, as a `java.util.List` — already the representation of an Eliot `List`, so
    * the native is a bare `GETSTATIC`.
    */
  val argumentsField: String = "eliot$arguments"

  val argumentsFieldDescriptor: String = "Ljava/util/List;"

  private val JString      = "java/lang/String"
  private val JObject      = "java/lang/Object"
  private val JBoolean     = "java/lang/Boolean"
  private val JBigInteger  = "java/math/BigInteger"
  private val JSystem      = "java/lang/System"
  private val JPath        = "java/nio/file/Path"
  private val JFile        = "java/io/File"
  private val JList        = "java/util/List"
  private val JProcess     = "java/lang/Process"
  private val JProcessBldr = "java/lang/ProcessBuilder"
  private val JRedirect    = "java/lang/ProcessBuilder$Redirect"
  private val JInputStream = "java/io/InputStream"
  private val JCharset     = "java/nio/charset/Charset"
  private val JThrowable   = "java/lang/Throwable"
  private val ObjectArray  = "[Ljava/lang/Object;"

  /** The generic natives, with the erased signature both definition and call sites resolve through. */
  val genericNativeSignatures: Map[ValueFQN, GenericNativeSignature] = Map(
    environmentFqn("isNull") -> GenericNativeSignature(Seq(systemAnyValue), boolType),
    processFqn("isNull")     -> GenericNativeSignature(Seq(systemAnyValue), boolType)
  )

  /** The leaf natives keyed by their `Default`-qualified FQN, folded into [[NativeImplementation.implementations]]. */
  val implementations: Seq[(ValueFQN, NativeImplementation)] = Seq(
    // --- eliot.system.Environment ---
    entry(environmentFqn("argumentsInternal"), Seq.empty, listType, impure = true)(arguments),
    entry(environmentFqn("environmentVariableInternal"), Seq(stringType), stringType, impure = true)(
      environmentVariable
    ),
    entry(environmentFqn("workingDirectoryInternal"), Seq.empty, pathType, impure = true)(workingDirectory),
    genericEntry(environmentFqn("isNull"))(isNull),
    // --- eliot.system.Process ---
    entry(processFqn("runInternal"), Seq(listType, pathType), outcomeType, impure = true)(runCapturing),
    entry(processFqn("runInheritingIoInternal"), Seq(listType, pathType), outcomeType, impure = true)(runInheriting),
    entry(processFqn("outcomeErrorMessage"), Seq(outcomeType), stringType)(slot(0, JString)),
    entry(processFqn("outcomeExitCode"), Seq(outcomeType), intType)(slot(1, JBigInteger)),
    entry(processFqn("outcomeStandardOutput"), Seq(outcomeType), stringType)(slot(2, JString)),
    entry(processFqn("outcomeStandardError"), Seq(outcomeType), stringType)(slot(3, JString)),
    genericEntry(processFqn("isNull"))(isNull)
  )

  // --------------------------------------------------------------------------------------------------------------
  // Registration helpers — the FileNatives shapes
  // --------------------------------------------------------------------------------------------------------------

  private def entry(fqn: ValueFQN, params: Seq[ValueFQN], ret: ValueFQN, impure: Boolean = false)(
      body: MethodVisitor => Unit
  ): (ValueFQN, NativeImplementation) = {
    val isImpure = impure
    fqn -> new NativeImplementation {
      override val impure: Boolean                                      = isImpure
      override def generateMethod(cg: ClassGenerator): CompilerIO[Unit] =
        cg.createMethod[CompilerIO](JvmIdentifier(fqn.name.name), params, ret).use(_.runNative(body))
    }
  }

  private def genericEntry(fqn: ValueFQN, impure: Boolean = false)(
      body: MethodVisitor => Unit
  ): (ValueFQN, NativeImplementation) = {
    val isImpure = impure
    val sig      = genericNativeSignatures(fqn)
    fqn -> new NativeImplementation {
      override val impure: Boolean                                      = isImpure
      override def generateMethod(cg: ClassGenerator): CompilerIO[Unit] =
        cg.createMethod[CompilerIO](JvmIdentifier(fqn.name.name), sig.parameterTypes, sig.returnType)
          .use(_.runNative(body))
    }
  }

  // --------------------------------------------------------------------------------------------------------------
  // Shared helpers
  // --------------------------------------------------------------------------------------------------------------

  private def isNull(mv: MethodVisitor): Unit = {
    val isNullL = new Label()
    val endL    = new Label()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitJumpInsn(IFNULL, isNullL)
    mv.visitFieldInsn(GETSTATIC, JBoolean, "FALSE", "Ljava/lang/Boolean;")
    mv.visitJumpInsn(GOTO, endL)
    mv.visitLabel(isNullL)
    mv.visitFieldInsn(GETSTATIC, JBoolean, "TRUE", "Ljava/lang/Boolean;")
    mv.visitLabel(endL)
  }

  /** Read slot `index` of the four-slot outcome holder, cast to `castTo`. */
  private def slot(index: Int, castTo: String)(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitTypeInsn(CHECKCAST, ObjectArray)
    mv.visitIntInsn(BIPUSH, index)
    mv.visitInsn(AALOAD)
    mv.visitTypeInsn(CHECKCAST, castTo)
  }

  // --------------------------------------------------------------------------------------------------------------
  // Environment
  // --------------------------------------------------------------------------------------------------------------

  private def arguments(mv: MethodVisitor): Unit =
    mv.visitFieldInsn(GETSTATIC, argumentsHolderClass, argumentsField, argumentsFieldDescriptor)

  private def environmentVariable(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESTATIC, JSystem, "getenv", s"(L$JString;)L$JString;", false)
  }

  private def workingDirectory(mv: MethodVisitor): Unit = {
    mv.visitLdcInsn("user.dir")
    mv.visitMethodInsn(INVOKESTATIC, JSystem, "getProperty", s"(L$JString;)L$JString;", false)
    mv.visitInsn(ICONST_0)
    mv.visitTypeInsn(ANEWARRAY, JString)
    mv.visitMethodInsn(INVOKESTATIC, JPath, "of", s"(L$JString;[L$JString;)L$JPath;", true)
    mv.visitMethodInsn(INVOKEINTERFACE, JPath, "toAbsolutePath", s"()L$JPath;", true)
    mv.visitMethodInsn(INVOKEINTERFACE, JPath, "normalize", s"()L$JPath;", true)
  }

  // --------------------------------------------------------------------------------------------------------------
  // Process
  // --------------------------------------------------------------------------------------------------------------

  /** `run`: capture both streams. */
  private def runCapturing(mv: MethodVisitor): Unit = runProcess(mv, capturing = true)

  /** `runInheritingIo`: hand the child this program's own streams. */
  private def runInheriting(mv: MethodVisitor): Unit = runProcess(mv, capturing = false)

  /** Build a `ProcessBuilder` over the command list and working directory, run it to completion, and leave the
    * four-slot outcome holder on the stack.
    *
    * Locals: 0 command list, 1 working directory, 2 the holder, 3 the `Process`, 4 scratch. Everything from creating
    * the builder to reading the exit code is inside one try/catch: a missing executable throws `IOException` and an
    * interrupted wait throws `InterruptedException`, and both mean the same thing to a caller — the program did not run
    * — so both land in the error slot with their message.
    */
  private def runProcess(mv: MethodVisitor, capturing: Boolean): Unit = {
    val tryStart = new Label()
    val tryEnd   = new Label()
    val handler  = new Label()
    val endL     = new Label()

    // holder = new Object[4]
    mv.visitIntInsn(BIPUSH, 4)
    mv.visitTypeInsn(ANEWARRAY, JObject)
    mv.visitVarInsn(ASTORE, 2)

    mv.visitTryCatchBlock(tryStart, tryEnd, handler, JThrowable)
    mv.visitLabel(tryStart)

    // builder = new ProcessBuilder(command).directory(workingDirectory.toFile())
    mv.visitTypeInsn(NEW, JProcessBldr)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitTypeInsn(CHECKCAST, JList)
    mv.visitMethodInsn(INVOKESPECIAL, JProcessBldr, "<init>", s"(L$JList;)V", false)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEINTERFACE, JPath, "toFile", s"()L$JFile;", true)
    mv.visitMethodInsn(INVOKEVIRTUAL, JProcessBldr, "directory", s"(L$JFile;)L$JProcessBldr;", false)

    if (!capturing) {
      // builder.inheritIO()
      mv.visitMethodInsn(INVOKEVIRTUAL, JProcessBldr, "inheritIO", s"()L$JProcessBldr;", false)
    }

    // process = builder.start()
    mv.visitMethodInsn(INVOKEVIRTUAL, JProcessBldr, "start", s"()L$JProcess;", false)
    mv.visitVarInsn(ASTORE, 3)

    // holder[2] / holder[3] = captured streams, or ""
    storeStream(mv, 2, capturing, "getInputStream")
    storeStream(mv, 3, capturing, "getErrorStream")

    // holder[1] = BigInteger.valueOf(process.waitFor())
    mv.visitVarInsn(ALOAD, 2)
    mv.visitInsn(ICONST_1)
    mv.visitVarInsn(ALOAD, 3)
    mv.visitMethodInsn(INVOKEVIRTUAL, JProcess, "waitFor", "()I", false)
    mv.visitInsn(I2L)
    mv.visitMethodInsn(INVOKESTATIC, JBigInteger, "valueOf", s"(J)L$JBigInteger;", false)
    mv.visitInsn(AASTORE)

    mv.visitLabel(tryEnd)
    mv.visitJumpInsn(GOTO, endL)

    // catch (Throwable t) { holder[0] = String.valueOf(t.getMessage()); holder[1] = BigInteger.ZERO; ... }
    mv.visitLabel(handler)
    mv.visitVarInsn(ASTORE, 4)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitInsn(ICONST_0)
    mv.visitVarInsn(ALOAD, 4)
    mv.visitMethodInsn(INVOKEVIRTUAL, JThrowable, "toString", s"()L$JString;", false)
    mv.visitInsn(AASTORE)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitInsn(ICONST_1)
    mv.visitFieldInsn(GETSTATIC, JBigInteger, "ZERO", s"L$JBigInteger;")
    mv.visitInsn(AASTORE)
    storeEmpty(mv, 2)
    storeEmpty(mv, 3)

    mv.visitLabel(endL)
    mv.visitVarInsn(ALOAD, 2)
  }

  /** `holder[index] = new String(process.<stream>().readAllBytes(), Charset.defaultCharset())`, or `""` when the
    * streams were inherited and there is nothing to read.
    */
  private def storeStream(mv: MethodVisitor, index: Int, capturing: Boolean, accessor: String): Unit =
    if (!capturing) storeEmpty(mv, index)
    else {
      mv.visitVarInsn(ALOAD, 2)
      mv.visitIntInsn(BIPUSH, index)
      mv.visitTypeInsn(NEW, JString)
      mv.visitInsn(DUP)
      mv.visitVarInsn(ALOAD, 3)
      mv.visitMethodInsn(INVOKEVIRTUAL, JProcess, accessor, s"()L$JInputStream;", false)
      mv.visitMethodInsn(INVOKEVIRTUAL, JInputStream, "readAllBytes", "()[B", false)
      mv.visitMethodInsn(INVOKESTATIC, JCharset, "defaultCharset", s"()L$JCharset;", false)
      mv.visitMethodInsn(INVOKESPECIAL, JString, "<init>", s"([BL$JCharset;)V", false)
      mv.visitInsn(AASTORE)
    }

  private def storeEmpty(mv: MethodVisitor, index: Int): Unit = {
    mv.visitVarInsn(ALOAD, 2)
    mv.visitIntInsn(BIPUSH, index)
    mv.visitLdcInsn("")
    mv.visitInsn(AASTORE)
  }
}
