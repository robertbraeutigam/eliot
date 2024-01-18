package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.{File, FilenameFilter}

class WalkSourcePaths extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact[_])(using process: CompilationProcess): IO[Unit] = fact match {
    case SourcePath(path) =>
      for {
        isFile      <- IO(path.isFile)
        _           <- process.registerFact(SourceFile(path)).whenA(isFile)
        isDirectory <- IO(path.isDirectory)
        _           <- process.registerFacts(
                         Option(path.listFiles((dir, name) => !name.startsWith(".")))
                           .map(_.toSeq)
                           .getOrElse(Seq.empty)
                           .map(SourcePath.apply)
                       )
        _           <- error(s"path $path does not represent a file or directory").whenA(!isFile && !isDirectory)
      } yield ()
    case _                => IO.unit
  }
}
