package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.CompilerSignal

import java.io.File

/** Path to a source file or directory containing source files.
  */
case class SourcePath(path: File) extends CompilerSignal
