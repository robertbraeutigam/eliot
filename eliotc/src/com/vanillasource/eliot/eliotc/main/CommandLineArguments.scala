package com.vanillasource.eliot.eliotc.main

import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName}

import java.io.File

case class CommandLineArguments(
    mainFunction: FunctionFQN = FunctionFQN(ModuleName(Seq.empty, "Main"), "main"),
    paths: Seq[File] = Seq.empty
)
