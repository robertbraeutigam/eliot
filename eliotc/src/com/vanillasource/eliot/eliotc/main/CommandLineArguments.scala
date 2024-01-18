package com.vanillasource.eliot.eliotc.main

import java.io.File

case class CommandLineArguments(
    paths: Seq[File] = Seq.empty
)
