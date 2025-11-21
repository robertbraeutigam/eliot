package com.vanillasource.eliot.eliotc.main

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}

import java.io.File
import java.nio.file.Path

case class CommandLineArguments(
    mainFunction: FunctionFQN = FunctionFQN(ModuleName(Seq.empty, "Main"), "main"),
    paths: Seq[File] = Seq.empty,
    targetPath: Path = Path.of("target")
)
