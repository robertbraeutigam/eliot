package com.vanillasource.eliot.eliotc.module.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.DataDefinition
import com.vanillasource.eliot.eliotc.module.fact.{ModuleData, TypeFQN, UnifiedModuleData}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class UnifiedModuleDataProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess): IO[Unit] =
    factKey match {
      case UnifiedModuleData.Key(tfqn) => unify(tfqn).getOrUnit
      case _                           => IO.unit
    }

  private def unify(tfqn: TypeFQN)(using process: CompilationProcess): OptionT[IO, Unit] =
    for {
      files       <- process.getFact(PathScan.Key(pathName(tfqn.moduleName))).toOptionT.map(_.files)
      allData     <- files.traverse(file => process.getFact(ModuleData.Key(file, tfqn))).map(_.sequence).toOptionT
      unifiedData <- unifyData(allData)
      _           <- process.registerFact(unifiedData).liftOptionT
    } yield ()

  private def unifyData(data: Seq[ModuleData])(using CompilationProcess): OptionT[IO, UnifiedModuleData] =
    if (hasMoreImplementations(data)) {
      registerCompilerError(data.head.dataDefinition.name.as("Has multiple implementations.")).liftOptionTNone
    } else if (!hasSameSignatures(data)) {
      registerCompilerError(data.head.dataDefinition.name.as("Has multiple different definitions.")).liftOptionTNone
    } else {
      // FIXME: arguments not given is not supported yet
      val implementedData = data.find(_.dataDefinition.arguments.nonEmpty).getOrElse(data.head)
      UnifiedModuleData(implementedData.tfqn, implementedData.typeDictionary, implementedData.dataDefinition)
        .pure[IO]
        .liftOptionT
    }

  private def hasMoreImplementations(datas: Seq[ModuleData]): Boolean = {
    // FIXME: this too relies on count, it should be an Option
    datas.count(_.dataDefinition.arguments.nonEmpty) > 1
  }

  private def hasSameSignatures(datas: Seq[ModuleData]): Boolean = {
    val first = datas.head

    datas.tail.forall(data => DataDefinition.signatureEquality.eqv(first.dataDefinition, data.dataDefinition))
  }
}
