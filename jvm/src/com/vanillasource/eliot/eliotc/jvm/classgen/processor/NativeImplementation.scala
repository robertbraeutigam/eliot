package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemLangType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.Opcodes

trait NativeImplementation {
  def generateMethod(mainClassGenerator: ClassGenerator): CompilerIO[Unit]
}

object NativeImplementation {
  val implementations: Map[FunctionFQN, NativeImplementation] = Map.from(
    Seq(
      (systemLangFunction("String", "printlnInternal"), eliot_lang_String_printlnInternal),
      (systemLangFunction("Unit", "unit"), eliot_lang_Unit_unit)
    )
  )

  private def systemLangFunction(moduleName: String, functionName: String): FunctionFQN =
    FunctionFQN(ModuleName(defaultSystemPackage, moduleName), functionName)

  private def eliot_lang_Unit_unit: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO]("unit", Seq.empty, systemLangType("Unit"))
        .use { methodGenerator =>
          methodGenerator.runNative { methodVisitor =>
            methodVisitor.visitInsn(Opcodes.ACONST_NULL)
          }
        }
    }
  }

  private def eliot_lang_String_printlnInternal: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO]("printlnInternal", Seq(systemLangType("String")), systemLangType("Unit"))
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
}
