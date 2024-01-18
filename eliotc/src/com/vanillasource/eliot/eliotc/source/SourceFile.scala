package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.CompilerSignal

import java.io.File

case class SourceFile(path: File) extends CompilerSignal
