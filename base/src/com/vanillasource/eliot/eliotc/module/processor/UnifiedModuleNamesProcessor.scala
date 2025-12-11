package com.vanillasource.eliot.eliotc.module.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class UnifiedModuleNamesProcessor extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] =
    factKey match {
      case UnifiedModuleNames.Key(moduleName) => unifyModules(moduleName).getOrUnit
      case _                                  => IO.unit
    }

  private def unifyModules(name: ModuleName)(using CompilationProcess): OptionT[IO, Unit] =
    for {
      files    <- getFact(PathScan.Key(pathName(name))).toOptionT.map(_.files)
      allNames <- files.traverse(file => getFact(ModuleNames.Key(file))).map(_.sequence).toOptionT
      _        <-
        debug[OptionTIO](
          s"Unified ${name.show} has functions: ${allNames.map(_.functionNames).flatten.toSet.mkString(", ")}, data: ${allNames.map(_.typeNames).flatten.toSet.mkString(", ")}"
        )
      _        <- registerFact(
                    UnifiedModuleNames(
                      name,
                      allNames.map(_.functionNames).flatten.toSet,
                      allNames.map(_.typeNames).flatten.toSet
                    )
                  ).liftOptionT
    } yield ()
}
