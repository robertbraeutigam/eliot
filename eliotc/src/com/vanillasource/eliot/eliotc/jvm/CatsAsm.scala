package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.jvm.GeneratedModule.ClassFile
import com.vanillasource.eliot.eliotc.jvm.NativeType.javaSignatureName
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

object CatsAsm {
  def createClassGenerator(name: ModuleName): IO[ClassGenerator] = IO {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
      name.name, // TODO: all class names are legal here?
      null,
      "java/lang/Object",
      null
    )

    new ClassGenerator(name, classWriter)
  }

  private def calculateSignatureString(signatureTypes: Seq[TypeFQN]): String =
    s"(${signatureTypes.init.map(javaSignatureName).mkString})${javaSignatureName(signatureTypes.last)}"

  class ClassGenerator(val name: ModuleName, val classWriter: ClassWriter) {
    def generate(): IO[ClassFile] = {
      val pathName  = if (name.packages.isEmpty) "" else name.packages.mkString("", "/", "/")
      val entryName = name.name + ".class" // FIXME: same javaname conversion as in class! Use the class name!

      IO(classWriter.visitEnd()) >> IO.pure(ClassFile(pathName + entryName, classWriter.toByteArray))
    }

    def createMethod(name: String, signatureTypes: Seq[TypeFQN]): Resource[IO, MethodGenerator] = Resource.make(IO {
      val methodVisitor = classWriter.visitMethod(
        Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
        name, // TODO: can every method name be converted to Java?
        calculateSignatureString(signatureTypes),
        null,
        null
      )

      methodVisitor.visitCode()

      MethodGenerator(methodVisitor)
    })(methodGenerator =>
      IO {
        // TODO: add more types (primitives)
        if (signatureTypes.last === systemUnitType) {
          methodGenerator.methodVisitor.visitInsn(Opcodes.RETURN)
        } else {
          methodGenerator.methodVisitor.visitInsn(Opcodes.ARETURN)
        }

        methodGenerator.methodVisitor.visitMaxs(0, 0)
        methodGenerator.methodVisitor.visitEnd()
      }
    )
  }

  class MethodGenerator(val methodVisitor: MethodVisitor) {
    def addLdcInsn(value: Object): IO[Unit] = IO {
      methodVisitor.visitLdcInsn(value)
    }

    def addCallTo(calledFfqn: FunctionFQN, callSignature: Seq[TypeFQN]): IO[Unit] = IO {
      methodVisitor.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        calledFfqn.moduleName.packages
          .appended(calledFfqn.moduleName.name)
          .mkString("/"),
        calledFfqn.functionName, // TODO: not a safe name
        calculateSignatureString(callSignature),
        false
      )
    }

    def runNative(block: MethodVisitor => Unit): IO[Unit] = IO {
      block(methodVisitor)
    }
  }
}
