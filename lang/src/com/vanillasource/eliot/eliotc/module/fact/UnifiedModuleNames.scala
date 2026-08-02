package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The flattened name set of a module in one platform pool.
  *
  * The fact is *total*: [[com.vanillasource.eliot.eliotc.module.processor.UnifiedModuleNamesProcessor]] answers with an
  * empty, `present = false` fact for a module that resolves in no mount of the pool rather than declining, so readers
  * request it with `getFactOrAbort` and inspect the payload. A membership probe reads `names.contains(...)`, which is
  * correctly `false` for an absent module; the two readers that must tell an *absent* module apart from a *present but
  * empty* one (import resolution, the jvm entry-point pre-flight) inspect [[present]]. Keeping the fact total removes the
  * cache-poison edge a decline would leave behind (an absent module recomputes to the same equality-stable empty value,
  * so its readers stop regenerating every warm build — see `docs/retire-optional-fact-reads.md`).
  *
  * @param present
  *   `true` when the module resolves in at least one mount of [[platform]]'s pool; `false` for the totalized absent case
  *   (which always carries an empty [[names]]).
  */
case class UnifiedModuleNames(
    moduleName: ModuleName,
    names: Map[QualifiedName, Visibility],
    present: Boolean,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleNames] = UnifiedModuleNames.Key(moduleName, platform)
}

object UnifiedModuleNames {
  case class Key(moduleName: ModuleName, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[UnifiedModuleNames]
}
