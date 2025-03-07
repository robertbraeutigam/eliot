package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.{File, FilenameFilter}

class WalkSourcePaths extends CompilerProcessor with Logging with User {
  override def process(fact: CompilerFact)(using process: CompilationProcess): IO[Unit] = fact match {
    case SourcePath(path, rootPath) =>
      for {
        isFile      <- IO(path.isFile)
        _           <- process.registerFact(SourceFile(path, rootPath)).whenA(isFile)
        isDirectory <- IO(path.isDirectory)
        _           <- process.registerFacts(
                         Option(path.listFiles((_, name) => !name.startsWith(".")))
                           .map(_.toSeq)
                           .getOrElse(Seq.empty)
                           .map(SourcePath(_, rootPath))
                       )
        _           <- compilerGlobalError(s"Path $path does not represent a file or directory.").whenA(!isFile && !isDirectory)
      } yield ()
    case _                          => IO.unit
  }
}
