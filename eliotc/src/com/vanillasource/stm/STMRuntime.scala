package com.vanillasource.stm

import cats.effect.IO
import io.github.timwspence.cats.stm.STM as CatsSTM

case class STMRuntime private[stm] (catsSTM: CatsSTM[IO])
