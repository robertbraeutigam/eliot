package com.vanillasource.stm

import cats.effect.IO
import cats.free.Free
import cats.~>
import com.vanillasource.stm.STMOps.*
import io.github.timwspence.cats.stm.STM as CatsSTM

type STM[A] = Free[STMOps, A]

object STM {
  def createRuntime(): IO[STMRuntime] = CatsSTM.runtime[IO].map(STMRuntime.apply)

  def createSTMVar[T](initialValue: T): STM[STMVar[T]] = Free.liftF(CreateSTMVar(initialValue))

  def retry[A](): STM[A] = Free.liftF(Retry())

  def raiseError[A](t: Throwable): STM[A] = Free.liftF(Error(t))

  extension [A](stm: STM[A]) {
    def commit(using stmRuntime: STMRuntime): IO[A] =
      stmRuntime.catsSTM.commit(stm.foldMap(stmToTxn(stmRuntime.catsSTM)))
  }

  private def stmToTxn(catsSTM: CatsSTM[IO]): STMOps ~> catsSTM.Txn = new (STMOps ~> catsSTM.Txn) {
    import catsSTM.*

    override def apply[A](fa: STMOps[A]): catsSTM.Txn[A] = fa match
      case CreateSTMVar(initialValue)  => TVar.of(initialValue).map(CatsSTMVar.apply)
      case GetSTMVar(stmVar)           => stmVar.asInstanceOf[CatsSTMVar[A]].tvar.get
      case SetSTMVar(stmVar, newValue) => stmVar.asInstanceOf[CatsSTMVar[Any]].tvar.set(newValue)
      case Retry()                     => catsSTM.retry
      case Error(t)                    => catsSTM.raiseError(t)

    private case class CatsSTMVar[A](tvar: catsSTM.TVar[A]) extends STMVar[A] {
      def get(): STM[A]        = Free.liftF(GetSTMVar(this))
      def set(a: A): STM[Unit] = Free.liftF(SetSTMVar(this, a))
    }
  }
}
