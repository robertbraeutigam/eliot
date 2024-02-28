package com.vanillasource.eliot.eliotc.output

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.ModuleName
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.{BufferedOutputStream, FileOutputStream}

class OutputWriter extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case Output(moduleName, content) => writeFile(moduleName, content)
    case _                           => IO.unit

  private def writeFile(moduleName: ModuleName, content: Array[Byte]) =
    info(s"writing output for ${moduleName.show}") >>
      fileResource(moduleName).use(bos => IO.blocking(bos.write(content)))

  private def fileResource(moduleName: ModuleName) = for {
    fos <- Resource.fromAutoCloseable(IO.blocking(new FileOutputStream(s"${moduleName.name}.bin")))
    bos <- Resource.fromAutoCloseable(IO(new BufferedOutputStream(fos)))
  } yield bos
}
