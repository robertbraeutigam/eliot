package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class FactCacheTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import FactCacheTest.*

  private val sample =
    FactCacheData(
      FactCache.CACHE_VERSION,
      Map(
        CacheKey("a") -> CacheEntry(CacheFact("a", 1), Set(CacheKey("b"))),
        CacheKey("b") -> CacheEntry(CacheFact("b", 2), Set.empty)
      )
    )

  "FactCache" should "round-trip saved data through disk" in {
    withTempDir { dir =>
      FactCache.save(dir, sample) >> FactCache.load(dir)
    }.asserting(_ shouldBe Some(sample))
  }

  it should "return None when no cache file exists" in {
    withTempDir(FactCache.load).asserting(_ shouldBe None)
  }

  it should "discard a cache whose version does not match" in {
    val stale = sample.copy(version = FactCache.CACHE_VERSION + 1)
    withTempDir { dir =>
      FactCache.save(dir, stale) >> FactCache.load(dir)
    }.asserting(_ shouldBe None)
  }

  it should "return None for a corrupt cache file" in {
    withTempDir { dir =>
      IO.blocking(Files.write(FactCache.cacheFile(dir), "not a serialized cache".getBytes(StandardCharsets.UTF_8))) >>
        FactCache.load(dir)
    }.asserting(_ shouldBe None)
  }

  it should "round-trip a fact carrying a (non-serializable) java.nio.file.Path" in {
    val withPath =
      FactCacheData(FactCache.CACHE_VERSION, Map(PathKey("p") -> CacheEntry(PathFact(Path.of("/tmp/some/file.els")), Set.empty)))
    withTempDir { dir =>
      FactCache.save(dir, withPath) >> FactCache.load(dir)
    }.asserting(_ shouldBe Some(withPath))
  }

  it should "drop a non-serializable fact but keep the serializable ones" in {
    val mixed = FactCacheData(
      FactCache.CACHE_VERSION,
      Map(
        CacheKey("good") -> CacheEntry(CacheFact("good", 1), Set.empty),
        CacheKey("bad")  -> CacheEntry(BadFact(new NotSerializable), Set.empty)
      )
    )
    withTempDir { dir =>
      FactCache.save(dir, mixed) >> FactCache.load(dir)
    }.asserting(_.map(_.entries.keySet) shouldBe Some(Set(CacheKey("good"))))
  }

  it should "not fail the build when a fact is not serializable" in {
    val unserializable = FactCacheData(FactCache.CACHE_VERSION, Map(CacheKey("x") -> CacheEntry(BadFact(new NotSerializable), Set.empty)))
    withTempDir { dir =>
      FactCache.save(dir, unserializable).attempt
    }.asserting(_ shouldBe Right(()))
  }
}

object FactCacheTest {
  case class CacheFact(name: String, value: Int) extends CompilerFact {
    override def key(): CompilerFactKey[CacheFact] = CacheKey(name)
  }
  case class CacheKey(name: String) extends CompilerFactKey[CacheFact]

  /** A plain class that is not `Serializable` (mirrors the closures inside `SemValue`). */
  class NotSerializable

  /** A fact carrying a genuinely non-serializable field, used to prove `save` drops it and stays fail-safe. */
  case class BadFact(resource: NotSerializable) extends CompilerFact {
    override def key(): CompilerFactKey[BadFact] = BadKey
  }
  case object BadKey extends CompilerFactKey[BadFact]

  /** A fact carrying a `java.nio.file.Path`, whose implementations are not `Serializable`. */
  case class PathFact(path: java.nio.file.Path) extends CompilerFact {
    override def key(): CompilerFactKey[PathFact] = PathKey(path.toString)
  }
  case class PathKey(name: String) extends CompilerFactKey[PathFact]

  def withTempDir[A](use: Path => IO[A]): IO[A] =
    IO.blocking(Files.createTempDirectory("eliot-cache-test"))
      .bracket(use) { dir =>
        IO.blocking {
          if (Files.exists(dir))
            Files.walk(dir).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(Files.delete(_))
        }
      }
}
