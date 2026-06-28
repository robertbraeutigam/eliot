# compiler — the compiler platform layer

This Mill module is the **compiler platform**: a source-only Eliot layer, peer to `jvm`, that implements the abstract
base (`lang` + `stdlib`) for the **compile-time phase** (Normalisation by Evaluation / type checking) — the way `jvm`
implements it for the runtime phase. See the "compiler is itself a platform" section of `.claude/CLAUDE.md`.

- It ships only `.els` sources under `resources/eliot/…`; it carries no Scala processors (CP3's
  `CompilerNativesProcessor` lives in `lang`).
- Its `resources/eliot` dir is listed on the **`--compiler-path`** of every type-checking entry point (the
  `examples.run` driver, the LSP server, the test harnesses) **unconditionally** — unlike runtime targets such as `jvm`,
  which are selected per build. It is never on the runtime/codegen path.
- It depends on `stdlib` so the full abstract base is available to it.

Contents: the compile-time `Either` carrier (`resources/eliot/eliot/lang/Either.els`) — concrete `data Either`,
`foldEither`, and the `implement Monad[Either[String]]` / `Throw[String, Either[String]]` instances. This is the
compile-time implementation reduced by the one NbE evaluator during checking; the abstract `type Either[E, A]` lives in
the `stdlib` base and the runtime `jvm` `Either` is unchanged and structurally identical, so the compiler overlay is
transparent for ordinary runtime uses (the `add` pattern).
