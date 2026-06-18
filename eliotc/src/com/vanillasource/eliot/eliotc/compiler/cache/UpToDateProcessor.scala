package com.vanillasource.eliot.eliotc.compiler.cache

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Leaf processor for [[UpToDate]]: it produces the same constant value every run. Because it has no dependencies the
  * incremental engine always recomputes it, and because the value is constant it always compares equal — so any fact
  * that depends on it (an input-less compiler constant, see [[UpToDate]]) is proven unchanged on the no-change path.
  */
class UpToDateProcessor extends SingleFactProcessor[UpToDate.Key] with Logging {
  override protected def generateSingleFact(key: UpToDate.Key): CompilerIO[UpToDate] = UpToDate().pure[CompilerIO]
}
