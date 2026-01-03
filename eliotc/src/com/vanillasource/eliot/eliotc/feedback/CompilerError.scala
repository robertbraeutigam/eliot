package com.vanillasource.eliot.eliotc.feedback

import com.vanillasource.eliot.eliotc.pos.PositionRange

case class CompilerError(message: String, description: Seq[String], source: String, sourceRange: PositionRange)

