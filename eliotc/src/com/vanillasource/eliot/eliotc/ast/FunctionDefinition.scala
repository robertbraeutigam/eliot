package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class FunctionDefinition(name: Sourced[Token], body: Sourced[FunctionBody])
