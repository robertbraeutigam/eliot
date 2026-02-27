package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import NativeType.{
  convertToCtorSignatureString,
  convertToMainClassName,
  convertToNestedClassName,
  convertToSignatureString,
  javaInternalName,
  javaSignatureName
}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.objectweb.asm.{MethodVisitor, Opcodes}

class MethodGenerator(private val internalName: String, val methodVisitor: MethodVisitor) {

  /** Add calling the given function with the given signature.
    * @param calledVfqn
    *   The called function's fully qualified name.
    */
  def addCallTo[F[_]: Sync](calledVfqn: ValueFQN, parameterTypes: Seq[ValueFQN], resultType: ValueFQN): F[Unit] =
    Sync[F].delay {
      methodVisitor.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        convertToMainClassName(calledVfqn.moduleName),
        JvmIdentifier.encode(calledVfqn.name.name).value,
        convertToSignatureString(parameterTypes, resultType),
        false
      )
    }

  /** Instantiate a class with its constructor. It is assumed that all parameters are correctly on the stack already.
    */
  def addCallToCtor[F[_]: Sync](target: ValueFQN, parameterTypes: Seq[ValueFQN]): F[Unit] = Sync[F].delay {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      convertToNestedClassName(target),
      "<init>",
      convertToCtorSignatureString(parameterTypes),
      false
    )
  }

  /** Call super's ctor. Assumed that it is loaded onto the stack.
    */
  def addCallToObjectCtor[F[_]: Sync](): F[Unit] = Sync[F].delay {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "java/lang/Object",
      "<init>",
      "()V",
      false
    )
  }

  /** Instantiate an object and leave it on stack.
    */
  def addNew[F[_]: Sync](target: ValueFQN): F[Unit] = Sync[F].delay {
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
  def addLoadVar[F[_]: Sync](varType: ValueFQN, index: Int): F[Unit] = Sync[F].delay {
    // TODO: Fix type ALOAD based on type
    methodVisitor.visitVarInsn(Opcodes.ALOAD, index)
  }

  /** Add getting the instance field from a data object.
    */
  def addGetField[F[_]: Sync](fieldName: JvmIdentifier, fieldType: ValueFQN, target: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitFieldInsn(
      Opcodes.GETFIELD,
      convertToNestedClassName(target),
      fieldName.value,
      javaSignatureName(fieldType)
    )
  }

  /** Add putting a value into a static field of this class.
    */
  def addPutStaticField[F[_]: Sync](fieldName: JvmIdentifier, fieldType: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitFieldInsn(
      Opcodes.PUTSTATIC,
      internalName,
      fieldName.value,
      javaSignatureName(fieldType)
    )
  }

  /** Add putting field into local instance variable.
    */
  def addPutField[F[_]: Sync](fieldName: JvmIdentifier, fieldType: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitFieldInsn(
      Opcodes.PUTFIELD,
      internalName,
      fieldName.value,
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
  def runNative[F[_]: Sync](block: MethodVisitor => Unit): F[Unit] = Sync[F].delay {
    block(methodVisitor)
  }

  def addCastTo[F[_]: Sync](targetType: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitTypeInsn(Opcodes.CHECKCAST, javaInternalName(targetType));
  }

  def addCallToApply[F[_]: Sync](): F[Unit] = Sync[F].delay {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKEINTERFACE,
      "java/util/function/Function",
      "apply",
      "(Ljava/lang/Object;)Ljava/lang/Object;",
      true
    )
  }

  def addGetStaticInstance[F[_]: Sync](singletonInternalName: String, interfaceDescriptor: String): F[Unit] =
    Sync[F].delay {
      methodVisitor.visitFieldInsn(
        Opcodes.GETSTATIC,
        singletonInternalName,
        "INSTANCE",
        interfaceDescriptor
      )
    }

  def addCallToAbilityMethod[F[_]: Sync](
      interfaceInternalName: String,
      methodName: JvmIdentifier,
      parameterTypes: Seq[ValueFQN],
      resultType: ValueFQN
  ): F[Unit] = Sync[F].delay {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKEINTERFACE,
      interfaceInternalName,
      methodName.value,
      convertToSignatureString(parameterTypes, resultType),
      true
    )
  }

  def addCallToVirtualMethod[F[_]: Sync](
      className: String,
      methodName: JvmIdentifier,
      parameterTypes: Seq[ValueFQN],
      resultType: ValueFQN
  ): F[Unit] = Sync[F].delay {
    methodVisitor.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      className,
      methodName.value,
      convertToSignatureString(parameterTypes, resultType),
      false
    )
  }

  def addConstNull[F[_]: Sync](): F[Unit] = Sync[F].delay {
    methodVisitor.visitInsn(Opcodes.ACONST_NULL)
  }
}
