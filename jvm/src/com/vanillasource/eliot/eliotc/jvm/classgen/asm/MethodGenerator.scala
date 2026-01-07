package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import NativeType.{convertToNestedClassName, convertToSignatureString, javaSignatureName}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemUnitType
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import org.objectweb.asm.{MethodVisitor, Opcodes}

class MethodGenerator(private val moduleName: ModuleName, val methodVisitor: MethodVisitor) {

  /** Add calling the given function with the given signature.
    * @param calledFfqn
    *   The called function's fully qualified moduleName.
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

  def addCastTo[F[_]: Sync](targetType: TypeFQN): F[Unit] = Sync[F].delay {
    methodVisitor.visitTypeInsn(Opcodes.CHECKCAST, javaSignatureName(targetType));
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
