package com.vanillasource.eliot.eliotc

import com.vanillasource.eliot.eliotc.CompilerSignal

/** Inserted when the compiler is started as the first and only automatically inserted fact. This fact can be used to
  * bootstrap initial processors.
  */
case object Init extends CompilerSignal
