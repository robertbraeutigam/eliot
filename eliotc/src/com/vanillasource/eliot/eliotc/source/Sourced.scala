package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.token.PositionRange

import java.io.File

/** A value of generic type transformed from a given snippet of code inside a given source code file.
  */
case class Sourced[+T](range: PositionRange, value: T)
