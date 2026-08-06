package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.compiler.cache.codec.ObjectId
import com.vanillasource.eliot.eliotc.processor.CompilerFact

/** What the cache holds for one fact's value, and — the part that matters — how it decides whether a *recomputed*
  * value is the same one (`docs/incremental-compilation.md` §1's equality cutoff, §13 step 4).
  *
  * The cutoff is where a warm build spends its validation: almost every fact is compared, not consumed. Doing that by
  * comparing values means materialising the stored side of every comparison, which is most of what a load costs. The
  * two implementations differ in exactly that:
  *
  *   - [[CachedValue.InMemory]] holds the fact and compares by value, which is all an in-memory or whole-graph cache
  *     can do.
  *   - [[CachedValue.Stored]] holds an [[ObjectId]] and compares **16 bytes**, hashing the recomputed value and never
  *     touching the stored side. Its fact is materialised only when something actually consumes it.
  *
  * `Stored` is deliberately not a `case class`: it carries the closures that read and hash, so structural equality
  * would compare those rather than what they stand for. Cache entries are compared for equality only by tests and by
  * the whole-graph backend, both of which deal in [[CachedValue.InMemory]].
  */
sealed trait CachedValue {

  /** Whether a value is held at all — answerable without materialising anything. */
  def isPresent: Boolean

  /** The fact, read on demand. */
  def materialise: Option[CompilerFact]

  /** Whether `fact`, freshly recomputed, is the value this stands for. */
  def matches(fact: CompilerFact): Boolean
}

object CachedValue {

  def of(value: Option[CompilerFact]): CachedValue = value.fold(Absent)(InMemory.apply)

  /** No value: a fact that declines to persist, or one validated structurally and never materialised. Its entry still
    * carries its edges, which is what the structural drill walks.
    */
  case object Absent extends CachedValue {
    override def isPresent: Boolean                    = false
    override def materialise: Option[CompilerFact]     = None
    override def matches(fact: CompilerFact): Boolean  = false
  }

  /** A value held as an object. Comparison is by value, with a reference check first because a fact accepted from
    * cache and handed straight back is the same instance.
    */
  final case class InMemory(fact: CompilerFact) extends CachedValue {
    override def isPresent: Boolean                   = true
    override def materialise: Option[CompilerFact]    = Some(fact)
    override def matches(other: CompilerFact): Boolean = (fact eq other) || fact == other
  }

  /** A value left in the store, identified by its content hash.
    *
    * @param id
    *   the stored value's [[ObjectId]]
    * @param read
    *   decodes it, on demand and at most once
    * @param identify
    *   hashes a recomputed value the same way the store did, so the two are comparable without decoding
    */
  final class Stored(val id: ObjectId, read: () => CompilerFact, identify: CompilerFact => ObjectId)
      extends CachedValue {
    private lazy val fact: CompilerFact = read()

    override def isPresent: Boolean                   = true
    override def materialise: Option[CompilerFact]    = Some(fact)
    override def matches(other: CompilerFact): Boolean = identify(other) == id
  }
}
