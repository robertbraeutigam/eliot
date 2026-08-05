package com.vanillasource.eliot.eliotc.plugin

case class Configuration(map: Map[Configuration.Key[?], Any] = Map.empty) {
  def set[T](key: Configuration.Key[T], value: T) =
    Configuration(map.updated(key, value))

  def contains(key: Configuration.Key[?]): Boolean = get(key).isDefined

  def get[T](key: Configuration.Key[T]): Option[T] =
    map.get(key).map(_.asInstanceOf[T])

  def getOrElse[T](key: Configuration.Key[T], defaultValue: => T): T =
    get(key).getOrElse(defaultValue)

  def updatedWith[T](key: Configuration.Key[T], updateFunction: Option[T] => Option[T]): Configuration =
    Configuration(map.updatedWith(key)(ov => updateFunction.apply(ov.map(_.asInstanceOf[T]))))

  /** Every key's contribution to the cache identity, as `(name, text)`, for the keys that participate (see
    * [[Configuration.Key.identityContribution]]). The fingerprint (`CacheFingerprint.config`) is computed from this, so
    * whether and how a value counts is decided by each key — never by an external exception list.
    */
  def identityContributions: Seq[(String, String)] =
    map.toSeq.flatMap { case (key, value) =>
      key.asInstanceOf[Configuration.Key[Any]].identityContribution(value).map(key.name -> _)
    }
}

object Configuration {

  /** A typed configuration key that is *also* the single authority on how its value participates in the cache identity.
    * The policy lives here, beside the flag that declares the key, rather than in a separate list of exceptions.
    */
  trait Key[T] {

    /** Stable identifier, and the sort key that makes the fingerprint order-independent. */
    def name: String

    /** How this key's value participates in the cache-identity fingerprint:
      *   - `None`       ⇒ it does not participate (a diagnostic flag, or an unrepresentable injected value);
      *   - `Some(text)` ⇒ `text` is folded into the fingerprint under [[name]].
      *
      * Total — every key answers — so a new key's author must make the decision, and a forgotten one falls to the safe
      * default (contribute ⇒ a cold build, never a stale cache).
      */
    def identityContribution(value: T): Option[String]
  }

  private final case class NamedKey[T](name: String, serialize: T => Option[String]) extends Key[T] {
    override def identityContribution(value: T): Option[String] = serialize(value)
  }

  /** A key whose value contributes its `toString` to the cache identity. The default for ordinary scalar settings
    * (`Path`, `String`, `Int`, `Boolean`, `ValueFQN`, `Seq[Path]`) whose `toString` is stable across runs.
    */
  def namedKey[T](name: String): Key[T] = NamedKey(name, v => Some(v.toString))

  /** A key that contributes a caller-chosen rendering of its value — the "what to contribute" case: a canonicalized
    * path, a normalized or subset form.
    */
  def namedKey[T](name: String)(serialize: T => String): Key[T] = NamedKey(name, v => Some(serialize(v)))

  /** A key that never contributes because it steers *observation*, not facts (`--statistics`, `--visualize-facts`). A
    * run that only toggles a diagnostic key reuses the warm cache.
    */
  def diagnosticKey[T](name: String): Key[T] = NamedKey(name, _ => None)

  /** A key that never contributes because its value has no stable representation (a function, a live mount) and is, by
    * construction, a deterministic function of keys that already contribute — so excluding it loses no identity.
    */
  def opaqueKey[T](name: String): Key[T] = NamedKey(name, _ => None)

  def stringKey(name: String): Key[String] = namedKey[String](name)
  def intKey(name: String): Key[Int]       = namedKey[Int](name)
}
