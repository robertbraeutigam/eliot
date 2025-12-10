package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.effect.Sync
import cats.data.OptionT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class UnifiedModuleNamesProcessor[F[_]: Sync] extends CompilerProcessor[F] with Logging {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess[F]): F[Unit] =
    factKey match {
      case UnifiedModuleNames.Key(moduleName) => unifyModules(moduleName).getOrUnit
      case _                                  => Monad[F].unit
    }

  private def unifyModules(name: ModuleName)(using process: CompilationProcess[F]): OptionT[F, Unit] =
    for {
      files    <- process.getFact(PathScan.Key(pathName(name))).toOptionT.map(_.files)
      allNames <- files.traverse(file => process.getFact(ModuleNames.Key(file))).map(_.sequence).toOptionT
      _        <-
        debug(
          s"Unified ${name.show} has functions: ${allNames.map(_.functionNames).flatten.toSet.mkString(", ")}, data: ${allNames.map(_.typeNames).flatten.toSet.mkString(", ")}"
        )
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
