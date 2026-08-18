package com.vanillasource.eliot.eliotc.compiler

import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

/** A diagnostic together with the fact generation that produced it.
  *
  * A run generates facts for two different reasons, and only one of them is the program being compiled. Besides the
  * facts the target actually demands, an incremental run re-runs facts purely to *ask whether they changed* — the
  * recompute-and-compare of [[IncrementalFactGenerator]]'s validation path, which walks the **previous** run's
  * dependency graph. Those generations are questions about the past, not about this program, and a question that can no
  * longer be answered ("the value this used to depend on is gone") is an answer — "changed" — not a compilation error.
  *
  * Recording which generation each diagnostic came from is what lets the run report only the diagnostics of the graph
  * it actually built ([[IncrementalFactGenerator.currentErrors]]). `source` is `None` for an engine invariant that
  * holds regardless of who asked — a key produced twice with different values is two paths disagreeing about one fact,
  * which is worth knowing about wherever it arises, validation included.
  */
case class AttributedError(source: Option[CompilerFactKey[?]], error: CompilerError)
