# Eliot Design Rules

- Getting facts should always be done with the expectation that
  the fact can be generated. If it can't, that should be an error.
  Therefore, use `getFactOrAbort` if there does not need to be an explicit
  error message, use `getFact` with immediate switch and `compileError` or `compilerAbort`
  in the None case.

