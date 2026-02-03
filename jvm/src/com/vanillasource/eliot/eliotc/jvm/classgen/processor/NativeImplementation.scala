package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemLangValue
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.Opcodes

trait NativeImplementation {
  def generateMethod(mainClassGenerator: ClassGenerator): CompilerIO[Unit]
}

object NativeImplementation {
  val implementations: Map[ValueFQN, NativeImplementation] = Map.from(
    Seq(
      (systemLangValueFQN("String", "printlnInternal"), eliot_lang_String_printlnInternal),
      (systemLangValueFQN("Unit", "unit"), eliot_lang_Unit_unit)
    )
  )

  private def systemLangValueFQN(moduleName: String, valueName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, moduleName), valueName)

  private def eliot_lang_Unit_unit: NativeImplementation = new NativeImplementation {
    override def generateMethod(classGenerator: ClassGenerator): CompilerIO[Unit] = {
      classGenerator
        .createMethod[CompilerIO]("unit", Seq.empty, systemLangValue("Unit"))
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
        .createMethod[CompilerIO]("printlnInternal", Seq(systemLangValue("String")), systemLangValue("Unit"))
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
