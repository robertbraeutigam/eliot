package com.vanillasource.eliot.eliotc.jvm.asm

import cats.effect.kernel.Resource
import cats.effect.{IO, Sync}
import cats.syntax.all.*
// FIXME: Can not refer to classgen!
import com.vanillasource.eliot.eliotc.jvm.classgen.NativeType.{convertToMainClassName, javaSignatureName}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

object CatsAsm {

  /** Generates an empty class for the given module. Each module has exactly one class generated for it.
    * @param name
    *   The module name to generate the class for.
    * @return
    */
  def createClassGenerator(name: ModuleName): IO[ClassGenerator] = IO {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
      convertToMainClassName(name), // TODO: all class names are legal here?
      null,
      "java/lang/Object",
      null
    )

    new ClassGenerator(name, classWriter)
  }

  // FIXME: This shouldn't be outside
  def calculateSignatureString(signatureTypes: Seq[TypeFQN]): String =
    s"(${signatureTypes.init.map(javaSignatureName).mkString})${javaSignatureName(signatureTypes.last)}"

  // FIXME: name is referred to from the outside!
  class ClassGenerator(val name: ModuleName, val classWriter: ClassWriter) {

    /** Generate the byte-code for the currently created class.
      */
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
          if (name === "<init>") Opcodes.ACC_PUBLIC else Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
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
