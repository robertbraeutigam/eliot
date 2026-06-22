package com.vanillasource.eliot.eliotc.monomorphize.unify

/** The outcome of a speculative ([[Unifier.tryUnify]]) unification (D5). Replaces the former
  * `errors.size`-delta idiom, where a caller detected unification success by diffing the unifier's mutable error
  * log before and after a [[Unifier.unify]] call — a brittle hand-rolled speculative transaction that was repeated
  * at four call sites across three files.
  *
  * `tryUnify` runs the unification but never commits a mismatch; the *caller* decides what a contradiction means
  * (pursue a `Coerce` insertion, accumulate a `Combine` candidate, re-postpone in [[Unifier.drain]], or finally
  * report the mismatch). Both cases carry the resulting [[Unifier]] — every metavariable solved along the way is
  * preserved — so the only thing the result distinguishes is whether the unification reached a contradiction.
  */
sealed trait UnifyResult {

  /** The unifier produced by the speculative unification. For [[Unified]] it carries every solution and is safe to
    * commit; for [[Contradiction]] it carries the same partial solutions but with the new mismatch errors stripped.
    */
  def unifier: Unifier
}

object UnifyResult {

  /** The two sides are definitionally equal (with any metavariables solved to make them so). [[unifier]] carries
    * those solutions and is safe to commit.
    */
  case class Unified(unifier: Unifier) extends UnifyResult

  /** The two sides are not definitionally equal. [[unifier]] carries whatever metavariables were solved before the
    * contradiction was reached, but with the new mismatch errors removed — the caller decides whether to report a
    * mismatch, attempt a coercion, accumulate a `Combine` candidate, or re-postpone. A caller that wants a clean
    * rollback simply discards it and keeps its own pre-unification state.
    */
  case class Contradiction(unifier: Unifier) extends UnifyResult
}
