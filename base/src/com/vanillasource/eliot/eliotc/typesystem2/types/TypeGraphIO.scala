package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.data.{StateT, WriterT}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

object TypeGraphIO {
  private type TypeCheckStateIO[T] = StateT[CompilerIO, TypeCheckState, T]
  type TypeGraphIO[T]              = WriterT[TypeCheckStateIO, SymbolicUnification, T]

  def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    WriterT.tell(constraint)

  def tellUniversalVar(name: String): TypeGraphIO[Unit] =
    tellConstraint(SymbolicUnification.universalVar(name))

  def liftState[T](stateIO: TypeCheckStateIO[T]): TypeGraphIO[T] =
    WriterT.liftF(stateIO)
}
