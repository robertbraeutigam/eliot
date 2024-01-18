package com.vanillasource.eliot.eliotc.token

import java.io.File

case class SourceTokens(file: File, tokens: Seq[Sourced[Token]])
