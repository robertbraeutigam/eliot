package com.vanillasource.eliot.eliotc.platform

/** The two-valued *phase* marker threaded through the front-end fact chain, from `PathScan` to `SaturatedValue` (the
  * "compiler as a platform" plan, CP1). It is a plain namespace tag on the fact keys — processors are otherwise
  * unchanged — that selects *which pool of source roots* a name is resolved against:
  *
  *   - [[Platform.Compiler]] scans the **whole runtime track plus each root's `eliot-compiler/` overlay** (a layer's
  *     opt-in compile-time contribution, override-preferred; only `stdlib` ships one) and drives **compile-time
  *     evaluation** (NbE / type checking);
  *   - [[Platform.Runtime]] scans the **runtime roots** (the base, the selected target such as `jvm`, and the user's
  *     program — every `--path`, without the overlays) and drives **codegen** (`used → uncurry → backend`).
  *
  * Only platform-provided concretes (the `Int` representation, the compiler-platform `Either`, …) ever differ between
  * the two pools; most names are abstract-only or user code and unify identically under both markers. The fact graph is
  * demand-driven, so a fact is computed per `(key, platform)` on demand and cached.
  *
  * [[Platform.Runtime]] is the default on every chain fact key: the main pipeline (base + target + user program) and
  * every downstream reader (monomorphize, ability resolution, codegen, the LSP server) resolve under it, so a reader
  * that does not care about the phase split simply omits the marker and gets the runtime pool. Compile-time-only
  * readers (CP3's `CompilerNativesProcessor`) ask for [[Platform.Compiler]] explicitly.
  */
enum Platform {
  case Compiler
  case Runtime
}
