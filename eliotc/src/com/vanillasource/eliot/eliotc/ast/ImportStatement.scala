package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class ImportStatement(
    keyword: Sourced[Token],
    packageNames: Seq[Sourced[Token]],
    moduleName: Sourced[Token]
)
