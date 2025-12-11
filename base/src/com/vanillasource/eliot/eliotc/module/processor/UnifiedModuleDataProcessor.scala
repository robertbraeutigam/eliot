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
      allData     <- files.traverse(file => process.getFact(ModuleData.Key(file, tfqn))).map(_.flatten).liftOptionT
      unifiedData <- unifyData(allData)
      _           <- process.registerFact(unifiedData).liftOptionT
    } yield ()

  private def unifyData(data: Seq[ModuleData])(using CompilationProcess): OptionT[IO, UnifiedModuleData] =
    if (data.isEmpty) {
      OptionT.none
    } else if (hasMoreImplementations(data)) {
      registerCompilerError(data.head.dataDefinition.name.as("Has multiple implementations.")).liftOptionTNone
    } else if (!hasSameSignatures(data)) {
      registerCompilerError(data.head.dataDefinition.name.as("Has multiple different definitions.")).liftOptionTNone
    } else {
      val implementedData = data.find(_.dataDefinition.fields.isDefined).getOrElse(data.head)
      UnifiedModuleData(implementedData.tfqn, implementedData.typeDictionary, implementedData.dataDefinition)
        .pure[IO]
        .liftOptionT
    }

  private def hasMoreImplementations(datas: Seq[ModuleData]): Boolean =
    datas.count(_.dataDefinition.fields.isDefined) > 1

  private def hasSameSignatures(datas: Seq[ModuleData]): Boolean = {
    val first = datas.head

    datas.tail.forall(data => DataDefinition.signatureEquality.eqv(first.dataDefinition, data.dataDefinition))
  }
}
