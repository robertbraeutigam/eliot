package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced

case class FunctionDefinition(name: Sourced[String], args: Seq[Sourced[String]], body: FunctionBody)
