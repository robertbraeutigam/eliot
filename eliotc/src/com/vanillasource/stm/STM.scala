package com.vanillasource.stm

import cats.data.{Kleisli, ReaderT}
import cats.effect.IO
import cats.free.Free
import cats.~>
import com.vanillasource.stm.STMOps._
import io.github.timwspence.cats.stm.STM as CatsSTM

type STM[A] = Free[STMOps, A]

object STM {
  def createRuntime(): IO[CatsSTM[IO]] = CatsSTM.runtime[IO]

  def createSTMVar[T](initialValue: T): STM[STMVar[T]] = Free.liftF(CreateSTMVar(initialValue))

  def retry[A](): STM[A] = Free.liftF(Retry())

  extension [A](stm: STM[A]) {
    def commit(using catsSTM: CatsSTM[IO]): IO[A] = catsSTM.commit(stm.foldMap(stmToTxn(catsSTM)))
  }

  private def stmToTxn(catsSTM: CatsSTM[IO]): STMOps ~> catsSTM.Txn = new (STMOps ~> catsSTM.Txn) {
    import catsSTM._

    override def apply[A](fa: STMOps[A]): catsSTM.Txn[A] = fa match
      case CreateSTMVar(initialValue)  => TVar.of(initialValue).map(CatsSTMVar.apply)
      case GetSTMVar(stmVar)           => stmVar.asInstanceOf[CatsSTMVar[A]].tvar.get
      case SetSTMVar(stmVar, newValue) => stmVar.asInstanceOf[CatsSTMVar[Any]].tvar.set(newValue)
      case Retry()                     => catsSTM.retry

    private case class CatsSTMVar[A](tvar: catsSTM.TVar[A]) extends STMVar[A] {
      def get(): STM[A]        = Free.liftF(GetSTMVar(this))
      def set(a: A): STM[Unit] = Free.liftF(SetSTMVar(this, a))
    }
  }
}
