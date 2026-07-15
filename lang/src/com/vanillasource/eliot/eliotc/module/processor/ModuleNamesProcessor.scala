package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Role}
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, NamedValue}
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ModuleNamesProcessor extends TransformationProcessor[CoreAST.Key, ModuleNames.Key](key => CoreAST.Key(key.uri)) {

  override protected def generateFromKeyAndFact(
      key: ModuleNames.Key,
      fact: CoreAST
  ): CompilerIO[ModuleNames] =
    extractNames(fact.ast.value.namedValues).map(names => ModuleNames(key.uri, fact.ast.as(names)))

  /** The module's **surface** name set — `Runtime` twins only. A `Signature` twin (the signature-split compile-time
    * value) is deliberately excluded: this set is the name-resolution dictionary and is enumerated/decoded by many
    * consumers (imports, ability/constructor indices, completion, the backend), and nothing ever *resolves into* a
    * signature twin. Signature twins remain first-class values through the value machinery — [[ModuleValue]] /
    * [[UnifiedModuleValue]] facts keyed by their role-bearing FQN (see [[ModuleValueProcessor]]).
    */
  private def extractNames(namedValues: Seq[NamedValue]): CompilerIO[Map[QualifiedName, Visibility]] =
    namedValues.filter(_.qualifiedName.value.role == Role.Runtime).foldLeftM(Map.empty[QualifiedName, Visibility]) {
      (acc, nv) =>
      val name = nv.qualifiedName.value

      if (acc.contains(name)) {
        compilerError(nv.qualifiedName.as("Name was already defined in this module.")).as(acc)
      } else {
        (acc + (name -> nv.visibility)).pure[CompilerIO]
      }
    }
}
