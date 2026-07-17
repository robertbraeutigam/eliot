package com.vanillasource.eliot.eliotc.namedvalues.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, Role, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.scan.PoolModules

/** Produces the [[NamedValuesIndex]] for one `(name, platform)`: over every module in the pool ([[PoolModules]]) it
  * reads the per-module [[UnifiedModuleNames]] (so layer copies are already merged) and keeps the public,
  * [[Qualifier.Default]], runtime-role names equal to `name`, emitting one [[ValueFQN]] each and sorting them by
  * canonical string.
  *
  * A pooled module that fails to produce its `UnifiedModuleNames` (e.g. a parse error) aborts the whole index via
  * [[getFactOrAbort]] — that module already reported its own error, so declining here is the fail-safe reaction (never
  * a silent partial index that would drop a matching value).
  */
class NamedValuesIndexProcessor extends SingleFactProcessor[NamedValuesIndex.Key] {

  override protected def generateSingleFact(key: NamedValuesIndex.Key): CompilerIO[NamedValuesIndex] =
    for {
      pool  <- getFactOrAbort(PoolModules.Key(key.platform))
      found <- pool.modules.toList.flatTraverse { module =>
                 getFactOrAbort(UnifiedModuleNames.Key(module, key.platform)).map { unified =>
                   unified.names.toList.collect {
                     case (qn, Visibility.Public)
                         if qn.name == key.name && qn.qualifier == Qualifier.Default && qn.role == Role.Runtime =>
                       ValueFQN(module, qn)
                   }
                 }
               }
    } yield NamedValuesIndex(key.name, key.platform, found.sortBy(_.show))
}
