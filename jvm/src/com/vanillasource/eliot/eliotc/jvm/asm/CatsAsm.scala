package com.vanillasource.eliot.eliotc.jvm.asm

import cats.effect.kernel.Resource
import cats.effect.{IO, Sync}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.jvm.asm.NativeType.{
  convertToMainClassName,
  convertToNestedClassName,
  convertToSignatureString,
  javaSignatureName
}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

object CatsAsm {

  /** Generates an empty class for the given module. Each module has exactly one class generated for it.
    * @param moduleName
    *   The module moduleName to generate the class for.
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

  class ClassGenerator(private val moduleName: ModuleName, private val classWriter: ClassWriter) {

    /** Generate the byte-code for the currently created class.
      */
    def generate(): IO[ClassFile] = {
      val pathName  = if (moduleName.packages.isEmpty) "" else moduleName.packages.mkString("", "/", "/")
      val entryName =
        moduleName.name + ".class" // FIXME: same javaname conversion as in class! Use the class moduleName!

      IO(classWriter.visitEnd()) >> IO.pure(ClassFile(pathName + entryName, classWriter.toByteArray))
    }

    /** Create inner class with the given non-qualified moduleName.
      * @param innerName
      *   The plain non-qualified and non-embedded moduleName of the inner class.
      * @return
      */
    def createInnerClassGenerator(innerName: String): IO[ClassGenerator] =
      for {
        classGenerator <- createClassGenerator(ModuleName(moduleName.packages, moduleName.name + "$" + innerName))
        _              <- IO(
                            classGenerator.classWriter.visitInnerClass(
                              moduleName.name + "$" + innerName,
                              moduleName.name,
                              innerName,
                              Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
                            )
                          )
        _              <- IO(
                            classWriter.visitInnerClass(
                              moduleName.name + "$" + innerName,
                              moduleName.name,
                              innerName,
                              Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
                            )
                          )
      } yield classGenerator

    /** Create the given field with the given type.
      * @param moduleName
      *   The moduleName of the field.
      * @param fieldType
      *   The type of the field.
      */
    def createField[F[_]: Sync](name: String, fieldType: TypeFQN): F[Unit] = Sync[F].delay {
      classWriter
        .visitField(
          Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL,
          name, // TODO: check moduleName
          javaSignatureName(fieldType),
          null,
          null
        )
        .visitEnd()
    }

    /** Create the given method with the given signature.
      * @param moduleName
      *   The simple non-qualified moduleName of the method.
      * @param signatureTypes
      *   The signature of the method, the last type being the return type.
      * @return
      */
    def createMethod[F[_]: Sync](
        name: String,
        parameterTypes: Seq[TypeFQN],
        resultType: TypeFQN
    ): Resource[F, MethodGenerator] =
      Resource.make(Sync[F].delay {
        val methodVisitor = classWriter.visitMethod(
          if (name === "<init>") Opcodes.ACC_PUBLIC else Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
          name, // TODO: can every method moduleName be converted to Java?
          convertToSignatureString(parameterTypes, resultType),
          null,
          null
        )

        methodVisitor.visitCode()

        MethodGenerator(moduleName, methodVisitor)
      })(methodGenerator =>
        Sync[F].delay {
          // TODO: add more types (primitives)
          if (resultType === systemUnitType) {
            methodGenerator.methodVisitor.visitInsn(Opcodes.RETURN)
          } else {
            methodGenerator.methodVisitor.visitInsn(Opcodes.ARETURN)
          }

          methodGenerator.methodVisitor.visitMaxs(0, 0)
          methodGenerator.methodVisitor.visitEnd()
        }
      )
  }

  class MethodGenerator(private val moduleName: ModuleName, val methodVisitor: MethodVisitor) {

    /** Add calling the given function with the given signature.
      * @param calledFfqn
      *   The called function's fully qualified moduleName.
      * @param callSignature
      *   The called function's signature, with the last type being the return type.
      */
    def addCallTo[F[_]: Sync](calledFfqn: FunctionFQN, parameterTypes: Seq[TypeFQN], resultType: TypeFQN): F[Unit] =
      Sync[F].delay {
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          calledFfqn.moduleName.packages
            .appended(calledFfqn.moduleName.name)
            .mkString("/"),
          calledFfqn.functionName, // TODO: not a safe moduleName
          convertToSignatureString(parameterTypes, resultType),
          false
        )
      }

    /** Instantiate a class with its constructor. It is assumed that all parameters are correctly on the stack already.
      */
    def addCallToCtor[F[_]: Sync](target: TypeFQN, parameterTypes: Seq[TypeFQN]): F[Unit] = Sync[F].delay {
      methodVisitor.visitMethodInsn(
        Opcodes.INVOKESPECIAL,
        convertToNestedClassName(target),
        "<init>",
        convertToSignatureString(parameterTypes, systemUnitType),
        false
      )
    }

    /** Instantiate an object and leave it on stack.
      */
    def addNew[F[_]: Sync](target: TypeFQN): F[Unit] = Sync[F].delay {
      methodVisitor.visitTypeInsn(
        Opcodes.NEW,
        convertToNestedClassName(target)
      )
      methodVisitor.visitInsn(Opcodes.DUP)
    }

    /** Add loading the given value onto the stack.
      */
    def addLdcInsn[F[_]: Sync](value: Object): F[Unit] = Sync[F].delay {
      methodVisitor.visitLdcInsn(value)
    }

    /** Load the given "variable" of given index to the stack.
      */
    def addLoadVar[F[_]: Sync](varType: TypeFQN, index: Int): F[Unit] = Sync[F].delay {
      // TODO: Fix type ALOAD based on type
      methodVisitor.visitVarInsn(Opcodes.ALOAD, index)
    }

    /** Add getting the instance field from a data object.
      */
    def addGetField[F[_]: Sync](fieldName: String, fieldType: TypeFQN, target: TypeFQN): F[Unit] = Sync[F].delay {
      methodVisitor.visitFieldInsn(
        Opcodes.GETFIELD,
        convertToNestedClassName(target),
        fieldName,
        javaSignatureName(fieldType)
      )
    }

    /** Add putting field into local instance variable.
      */
    def addPutField[F[_]: Sync](fieldName: String, fieldType: TypeFQN): F[Unit] = Sync[F].delay {
      methodVisitor.visitFieldInsn(
        Opcodes.PUTFIELD,
        moduleName.name,
        fieldName,
        javaSignatureName(fieldType)
      )
    }

    /** Load this (variable index 0). Works only in non-static methods.
      */
    def addLoadThis[F[_]: Sync](): F[Unit] = Sync[F].delay {
      methodVisitor.visitVarInsn(Opcodes.ALOAD, 0)
    }

    /** Add a native call to ASM.
      */
    // FIXME: Remove this and add all used features
    def runNative[F[_]: Sync](block: MethodVisitor => Unit): F[Unit] = Sync[F].delay {
      block(methodVisitor)
    }
  }
}
