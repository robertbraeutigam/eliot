# compiler — the compiler platform layer

This Mill module is the **compiler platform**: a source-only Eliot layer, peer to `jvm`, that implements the abstract
base (`lang` + `stdlib`) for the **compile-time phase** (Normalisation by Evaluation / type checking) — the way `jvm`
implements it for the runtime phase. See `docs/compiler-as-platform.md`.

- It ships only `.els` sources under `resources/eliot/…`; it carries no Scala processors (CP3's
  `CompilerNativesProcessor` lives in `lang`).
- Its `resources/eliot` dir is listed on the **`--compiler-path`** of every type-checking entry point (the
  `examples.run` driver, the LSP server, the test harnesses) **unconditionally** — unlike runtime targets such as `jvm`,
  which are selected per build. It is never on the runtime/codegen path.
- It depends on `stdlib` so the full abstract base is available to it.

Status (CP2): the module and its wiring exist; it is otherwise empty. It will hold CP4's `Either` carrier (concrete
`data Either`, `foldEither`, and the `Monad`/`Throw` instances) — the compile-time implementation reduced by the one NbE
evaluator during checking.
