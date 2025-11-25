package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

trait NativeImplementation {
  def generateMethod(classWriter: ClassWriter): IO[Unit]
}

object NativeImplementation {
  val implementations: Map[FunctionFQN, NativeImplementation] = Map.from(
    Seq(
      (systemLangFunction("String", "println"), eliot_lang_String_println)
    )
  )

  private def systemLangFunction(moduleName: String, functionName: String): FunctionFQN =
    FunctionFQN(ModuleName(defaultSystemPackage, moduleName), functionName)

  private def eliot_lang_String_println: NativeImplementation = new NativeImplementation {
    override def generateMethod(classWriter: ClassWriter): IO[Unit] =
      for {
        methodVisitor <- IO(
                           classWriter.visitMethod(
                             Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
                             "println",
                             "(Ljava/lang/String;)V",
                             null,
                             null
                           )
                         )
        _             <- IO(methodVisitor.visitCode())
        _             <- IO(
                           methodVisitor.visitFieldInsn(
                             Opcodes.GETSTATIC,
                             "java/lang/System",
                             "out",
                             "Ljava/io/PrintStream;"
                           )
                         )
        _             <- IO(methodVisitor.visitVarInsn(Opcodes.ALOAD, 0))
        _             <- IO(
                           methodVisitor.visitMethodInsn(
                             Opcodes.INVOKEVIRTUAL,
                             "java/io/PrintStream",
                             "println",
                             "(Ljava/lang/String;)V",
                             false
                           )
                         )
        _             <- IO(methodVisitor.visitInsn(Opcodes.RETURN))
        _             <- IO(methodVisitor.visitMaxs(0, 0))
        _             <- IO(methodVisitor.visitEnd())
      } yield ()
  }

}
