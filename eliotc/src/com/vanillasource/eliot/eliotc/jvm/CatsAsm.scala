package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import org.objectweb.asm.{ClassWriter, Opcodes}

object CatsAsm {
  def createClassWriter(name: ModuleName): IO[ClassWriter] = IO {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
      name.name, // TODO: all class names are legal here?
      null,
      "java/lang/Object",
      null
    )

    classWriter
  }

}
