package com.vanillasource.eliot.eliotc.jvm.asm

import cats.effect.{IO, Sync}
import cats.syntax.all.*
import cats.effect.kernel.Resource
import com.vanillasource.eliot.eliotc.jvm.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.asm.NativeType.{convertToMainClassName, convertToSignatureString, javaSignatureName}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}
import org.objectweb.asm.{ClassWriter, Opcodes}

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

object ClassGenerator {

  /** Generates an empty class for the given module. Each module has exactly one class generated for it.
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
}
