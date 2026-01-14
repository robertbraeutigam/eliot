package com.vanillasource.eliot.eliotc.module2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, NamedValue}
import com.vanillasource.eliot.eliotc.module2.fact.ModuleNames
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ModuleNamesProcessor extends TransformationProcessor[CoreAST.Key, ModuleNames.Key](key => CoreAST.Key(key.file)) {

  override protected def generateFromKeyAndFact(
      key: ModuleNames.Key,
      fact: CoreAST
  ): CompilerIO[ModuleNames] =
    extractNames(fact.ast.value.namedValues).map(names => ModuleNames(key.file, names))

  private def extractNames(namedValues: Seq[NamedValue]): CompilerIO[Set[String]] =
    namedValues.foldLeftM(Set.empty[String]) { (acc, nv) =>
      val name = nv.name.value

      if (acc.contains(name)) {
        compilerError(nv.name.as("Name was already defined in this module.")).as(acc)
      } else {
        (acc + name).pure[CompilerIO]
      }
    }
}
