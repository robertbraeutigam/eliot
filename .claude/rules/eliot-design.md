# Eliot Design Rules

- Getting facts should always be done with the expectation that the fact can be generated;
  a missing fact must never be silently ignored. The processor-facing API enforces this —
  there is no plain Option-returning `getFact`:
  - `getFactOrAbort` is the default read: absence aborts this computation. Correct both when
    absence "cannot happen" and when the upstream failure was already reported (producing no
    output is then the right reaction).
  - `getFactOrError(key)(error)` when the caller owns a user-facing message for the missing
    fact (e.g. "Could not find imported module").
  - `getFactIfProduced` only where absence is an expected, *handled* outcome by design — a
    producer that legitimately declines, or an upstream failure this computation deliberately
    proceeds without (skip a broken callee). Never use it to paper over a missing producer.
- A processor *declines* to produce a fact by explicit `abort` (possibly with zero errors).
  The engine treats an explicit abort as a decline; a generation that ends with no fact, no
  error, no abort, and no missing dependency read is an internal error.
