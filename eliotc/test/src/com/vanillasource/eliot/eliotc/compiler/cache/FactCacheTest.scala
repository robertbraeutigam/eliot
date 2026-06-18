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

  private val compilerFp = "compiler-1"
  private val configFp    = "config-1"

  private val sample =
    FactCacheData(
      FactCache.CACHE_VERSION,
      Map(
        CacheKey("a") -> CacheEntry(Some(CacheFact("a", 1)), Set(CacheKey("b"))),
        CacheKey("b") -> CacheEntry(Some(CacheFact("b", 2)), Set.empty)
      )
    )

  "FactCache" should "round-trip saved data through disk" in {
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, configFp, sample) >> FactCache.load(dir, compilerFp, configFp)
    }.asserting(_ shouldBe Some(sample))
  }

  it should "return None when no cache file exists" in {
    withTempDir(FactCache.load(_, compilerFp, configFp)).asserting(_ shouldBe None)
  }

  it should "discard a cache whose version does not match" in {
    val stale = sample.copy(version = FactCache.CACHE_VERSION + 1)
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, configFp, stale) >> FactCache.load(dir, compilerFp, configFp)
    }.asserting(_ shouldBe None)
  }

  it should "discard a cache saved by a different compiler fingerprint" in {
    withTempDir { dir =>
      FactCache.save(dir, "compiler-old", configFp, sample) >> FactCache.load(dir, "compiler-new", configFp)
    }.asserting(_ shouldBe None)
  }

  it should "discard a cache saved under a different configuration fingerprint" in {
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, "config-A", sample) >> FactCache.load(dir, compilerFp, "config-B")
    }.asserting(_ shouldBe None)
  }

  it should "return None for a corrupt cache file" in {
    withTempDir { dir =>
      IO.blocking(Files.write(FactCache.cacheFile(dir), "not a serialized cache".getBytes(StandardCharsets.UTF_8))) >>
        FactCache.load(dir, compilerFp, configFp)
    }.asserting(_ shouldBe None)
  }

  it should "round-trip a fact carrying a (non-serializable) java.nio.file.Path" in {
    val withPath =
      FactCacheData(
        FactCache.CACHE_VERSION,
        Map(PathKey("p") -> CacheEntry(Some(PathFact(Path.of("/tmp/some/file.els"))), Set.empty))
      )
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, configFp, withPath) >> FactCache.load(dir, compilerFp, configFp)
    }.asserting(_ shouldBe Some(withPath))
  }

  it should "keep the edges of a non-serializable fact but drop its value" in {
    val mixed = FactCacheData(
      FactCache.CACHE_VERSION,
      Map(
        CacheKey("good") -> CacheEntry(Some(CacheFact("good", 1)), Set.empty),
        BadKey           -> CacheEntry(Some(BadFact(new NotSerializable)), Set(CacheKey("good")))
      )
    )
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, configFp, mixed) >> FactCache.load(dir, compilerFp, configFp)
    }.asserting(
      _ shouldBe Some(
        FactCacheData(
          FactCache.CACHE_VERSION,
          Map(
            CacheKey("good") -> CacheEntry(Some(CacheFact("good", 1)), Set.empty),
            BadKey           -> CacheEntry(None, Set(CacheKey("good"))) // value dropped, edge kept
          )
        )
      )
    )
  }

  it should "not fail the build when a fact is not serializable" in {
    val unserializable =
      FactCacheData(FactCache.CACHE_VERSION, Map(BadKey -> CacheEntry(Some(BadFact(new NotSerializable)), Set.empty)))
    withTempDir { dir =>
      FactCache.save(dir, compilerFp, configFp, unserializable).attempt
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

  /** A fact carrying a genuinely non-serializable field, used to prove `save` drops the value but keeps the edge. */
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
