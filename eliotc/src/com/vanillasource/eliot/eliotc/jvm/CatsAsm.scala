package com.vanillasource.eliot.eliotc.jvm

import cats.effect.{Async, IO, Sync}
import cats.effect.kernel.Resource
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.jvm.GeneratedModule.ClassFile
import com.vanillasource.eliot.eliotc.jvm.NativeType.javaSignatureName
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference
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

    def createInnerClassGenerator(innerName: String): IO[ClassGenerator] =
      for {
        classGenerator <- createClassGenerator(ModuleName(name.packages, name.name + "$" + innerName))
        _              <- IO(
                            classGenerator.classWriter.visitInnerClass(
                              name.name + "$" + innerName,
                              name.name,
                              innerName,
                              Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
                            )
                          )
        _              <- IO(
                            classWriter.visitInnerClass(
                              name.name + "$" + innerName,
                              name.name,
                              innerName,
                              Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
                            )
                          )
      } yield classGenerator

    def createField[F[_]: Sync](name: String, fieldType: TypeFQN): F[Unit] = Sync[F].delay {
      classWriter
        .visitField(
          Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL,
          name, // TODO: check name
          javaSignatureName(fieldType),
          null,
          null
        )
        .visitEnd()
    }

    def createMethod[F[_]: Sync](name: String, signatureTypes: Seq[TypeFQN]): Resource[F, MethodGenerator] =
      Resource.make(Sync[F].delay {
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
        Sync[F].delay {
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
    def addCallTo[F[_]: Sync](calledFfqn: FunctionFQN, callSignature: Seq[TypeFQN]): F[Unit] = Sync[F].delay {
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

    def addLdcInsn[F[_]: Sync](value: Object): F[Unit] = Sync[F].delay {
      methodVisitor.visitLdcInsn(value)
    }

    def runNative[F[_]: Sync](block: MethodVisitor => Unit): F[Unit] = Sync[F].delay {
      block(methodVisitor)
    }
  }
}
