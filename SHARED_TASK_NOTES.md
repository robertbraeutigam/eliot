# monomorphize3 NbE Implementation - Task Notes

## Current State: ALL STEPS COMPLETE

All 10 steps of the NbE migration plan are fully implemented and verified.

- All monomorphize3 tests pass: 14 ProcessorTest + 61 TypeCheckTest + 1 ignored (nested HK - expected)
- Full parity audit complete: every monomorphize2 test has a corresponding monomorphize3 test
- `./mill __.compile` passes
- Code is formatted (scalafmt verified, no changes needed)
- Pre-existing failures in other packages (monomorphize2, eval, implementation) are unchanged and unrelated

## Only Outstanding Item

- **SKILL.md**: `.claude/skills/eliot-monomorphize3/SKILL.md` cannot be created due to sandbox write permissions blocking new subdirectories under `.claude/skills/`. A human should run `mkdir -p .claude/skills/eliot-monomorphize3/` and create the SKILL.md there, modeled after the existing `.claude/skills/eliot-monomorphize2/SKILL.md` but describing the NbE architecture. Multiple iterations have attempted this and been blocked by the same sandbox restriction.
