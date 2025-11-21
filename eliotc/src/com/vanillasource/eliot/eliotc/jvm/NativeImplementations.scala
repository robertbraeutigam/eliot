package com.vanillasource.eliot.eliotc.jvm

import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import org.objectweb.asm.{MethodVisitor, Opcodes}

trait NativeImplementations {
  def withArguments(methodVisitor: MethodVisitor, argumentsGenerator: => Unit): Unit
}

object NativeImplementations {
  val implementations: Map[FunctionFQN, NativeImplementations] = Map.from(
    Seq(
      (systemLangFunction("String", "println"), eliot_lang_String_println)
    )
  )

  private def systemLangFunction(moduleName: String, functionName: String): FunctionFQN =
    FunctionFQN(ModuleName(defaultSystemPackage, moduleName), functionName)

  private def eliot_lang_String_println: NativeImplementations = new NativeImplementations {
    override def withArguments(methodVisitor: MethodVisitor, argumentsGenerator: => Unit): Unit = {
      methodVisitor.visitFieldInsn(
        Opcodes.GETSTATIC,
        "java/lang/System",
        "out",
        "Ljava/io/PrintStream;"
      )

      argumentsGenerator

      methodVisitor.visitMethodInsn(
        Opcodes.INVOKEVIRTUAL,
        "java/io/PrintStream",
        "println",
        "(Ljava/lang/String;)V",
        false
      )
    }
  }
}
