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
}

object Configuration {
  trait Key[T]

  private case class NamedKey[T](name: String) extends Key[T]

  def namedKey[T](name: String): Key[T] = NamedKey[T](name)

  def stringKey(name: String): Key[String] = namedKey[String](name)
  def intKey(name: String): Key[Int]       = namedKey[Int](name)
}
