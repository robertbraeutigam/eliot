package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import cats.effect.std.Console
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST
import cats.Monad

class ModuleNamesProcessor[F[_]: {Monad, Console}]
    extends OneToOneProcessor((key: ModuleNames.Key) => DesugaredSourceAST.Key(key.file)) {
  override def generateFromKeyAndFact(key: ModuleNames.Key, fact: DesugaredSourceAST)(using
      process: CompilationProcess[F]
  ): F[Unit] =
    for {
      localFunctions <- extractLocalFunctions(fact.sourcedAst.value.functionDefinitions)
      localTypes     <- extractLocalTypes(fact.sourcedAst.value.typeDefinitions)
      _              <- process.registerFact(ModuleNames(key.file, localFunctions.keySet, localTypes.keySet))
    } yield ()
}
