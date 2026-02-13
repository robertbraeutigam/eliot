package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import NativeType.{
  convertToCtorSignatureString,
  convertToNestedClassName,
  convertToSignatureString,
  javaInternalName,
  javaSignatureName
}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.objectweb.asm.{MethodVisitor, Opcodes}

class MethodGenerator(private val moduleName: ModuleName, val methodVisitor: MethodVisitor) {

  /** Add calling the given function with the given signature.
    * @param calledVfqn
    *   The called function's fully qualified name.
    */
  def addCallTo[F[_]: Sync](calledVfqn: ValueFQN, parameterTypes: Seq[ValueFQN], resultType: ValueFQN): F[Unit] =
    Sync[F].delay {
      methodVisitor.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        calledVfqn.moduleName.packages
          .appended(calledVfqn.moduleName.name)
          .mkString("/"),
        calledVfqn.name.name,
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
  def addGetField[F[_]: Sync](fieldName: String, fieldType: ValueFQN, target: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitFieldInsn(
      Opcodes.GETFIELD,
      convertToNestedClassName(target),
      fieldName,
      javaSignatureName(fieldType)
    )
  }

  /** Add putting field into local instance variable.
    */
  def addPutField[F[_]: Sync](fieldName: String, fieldType: ValueFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitFieldInsn(
      Opcodes.PUTFIELD,
      moduleName.packages.appended(moduleName.name).mkString("/"), // TODO: use some existing naming method here
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
}
