package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import org.objectweb.asm.{MethodVisitor, Opcodes}

trait NativeImplementation {
  def withArguments(methodVisitor: MethodVisitor, argumentsGenerator: IO[Unit]): IO[Unit]
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
    override def withArguments(methodVisitor: MethodVisitor, argumentsGenerator: IO[Unit]): IO[Unit] =
      for {
        _ <- IO(
               methodVisitor.visitFieldInsn(
                 Opcodes.GETSTATIC,
                 "java/lang/System",
                 "out",
                 "Ljava/io/PrintStream;"
               )
             )

        _ <- argumentsGenerator

        _ <- IO(
               methodVisitor.visitMethodInsn(
                 Opcodes.INVOKEVIRTUAL,
                 "java/io/PrintStream",
                 "println",
                 "(Ljava/lang/String;)V",
                 false
               )
             )
      } yield ()
  }
}
