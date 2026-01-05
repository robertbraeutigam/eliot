package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.DataDefinition
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleData, TypeFQN, UnifiedModuleData}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class UnifiedModuleDataProcessor extends SingleFactProcessor[UnifiedModuleData.Key] {
  override protected def generateSingleFact(key: UnifiedModuleData.Key): CompilerIO[UnifiedModuleData] =
    for {
      pathScan    <- getFactOrAbort(PathScan.Key(pathName(key.tfqn.moduleName)))
      allData     <- pathScan.files
                       .traverse(file => getFactOrAbort(ModuleData.Key(file, key.tfqn)).attempt.map(_.toOption))
                       .map(_.flatten)
      unifiedData <- unifyData(key.tfqn, allData)
    } yield unifiedData

  private def unifyData(tfqn: TypeFQN, data: Seq[ModuleData]): CompilerIO[UnifiedModuleData] =
    if (data.isEmpty) {
      abort[UnifiedModuleData]
    } else if (hasMoreImplementations(data)) {
      compilerError(data.head.dataDefinition.name.as("Has multiple implementations.")) *> abort[
        UnifiedModuleData
      ]
    } else if (!hasSameSignatures(data)) {
      compilerError(data.head.dataDefinition.name.as("Has multiple different definitions.")) *> abort[
        UnifiedModuleData
      ]
    } else {
      val implementedData = data.find(_.dataDefinition.fields.isDefined).getOrElse(data.head)
      UnifiedModuleData(implementedData.tfqn, implementedData.typeDictionary, implementedData.dataDefinition)
        .pure[CompilerIO]
    }

  private def hasMoreImplementations(datas: Seq[ModuleData]): Boolean =
    datas.count(_.dataDefinition.fields.isDefined) > 1

  private def hasSameSignatures(datas: Seq[ModuleData]): Boolean = {
    val first = datas.head

    datas.tail.forall(data => DataDefinition.signatureEquality.eqv(first.dataDefinition, data.dataDefinition))
  }
}
