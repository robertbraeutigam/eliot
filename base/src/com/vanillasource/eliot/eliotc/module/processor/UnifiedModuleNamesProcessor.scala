package com.vanillasource.eliot.eliotc.module.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class UnifiedModuleNamesProcessor extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess): IO[Unit] =
    factKey match {
      case UnifiedModuleNames.Key(moduleName) => unifyModules(moduleName).getOrUnit
      case _                                  => IO.unit
    }

  private def unifyModules(name: ModuleName)(using process: CompilationProcess): OptionT[IO, Unit] =
    for {
      files    <- process.getFact(PathScan.Key(pathName(name))).toOptionT.map(_.files)
      allNames <- files.traverse(file => process.getFact(ModuleNames.Key(file))).map(_.sequence).toOptionT
      _        <-
        process
          .registerFact(
            UnifiedModuleNames(
              name,
              allNames.map(_.functionNames).flatten.toSet,
              allNames.map(_.typeNames).flatten.toSet
            )
          )
          .liftOptionT
    } yield ()
}
