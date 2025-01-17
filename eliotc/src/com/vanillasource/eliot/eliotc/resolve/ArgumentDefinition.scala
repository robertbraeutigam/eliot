package com.vanillasource.eliot.eliotc.resolve

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class ArgumentDefinition(name: Sourced[String], typeDefinition: TypeReference)
