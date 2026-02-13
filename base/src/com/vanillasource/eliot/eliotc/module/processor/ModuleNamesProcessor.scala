package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, NamedValue, QualifiedName}
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ModuleNamesProcessor extends TransformationProcessor[CoreAST.Key, ModuleNames.Key](key => CoreAST.Key(key.uri)) {

  override protected def generateFromKeyAndFact(
      key: ModuleNames.Key,
      fact: CoreAST
  ): CompilerIO[ModuleNames] =
    extractNames(fact.ast.value.namedValues).map(names => ModuleNames(key.uri, names))

  private def extractNames(namedValues: Seq[NamedValue]): CompilerIO[Set[QualifiedName]] =
    namedValues.foldLeftM(Set.empty[QualifiedName]) { (acc, nv) =>
      val name = nv.qualifiedName.value

      if (acc.contains(name)) {
        compilerError(nv.qualifiedName.as("Name was already defined in this module.")).as(acc)
      } else {
        (acc + name).pure[CompilerIO]
      }
    }
}
