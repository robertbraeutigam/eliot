package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.file.FileContent
import com.vanillasource.eliot.eliotc.source.stat.FileStat
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.net.URI
import java.time.Instant

class VirtualFileSystemTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val file = new File("/tmp/eliot-vfs/Test.els")
  private val uri  = file.toURI

  "VirtualFileSystem" should "serve the buffer content of an overridden file keyed by its URI" in {
    val vfs = new VirtualFileSystem
    vfs.update(uri, "buffer content")
    vfs.get(file).map(_.content) shouldBe Some("buffer content")
  }

  it should "match a file:/// URI override against the file:/ form of the same path" in {
    val vfs = new VirtualFileSystem
    vfs.update(URI.create("file:///tmp/eliot-vfs/Test.els"), "x")
    vfs.get(file).map(_.content) shouldBe Some("x")
  }

  it should "drop the override on remove, reverting to the on-disk file" in {
    val vfs = new VirtualFileSystem
    vfs.update(uri, "buffer content")
    vfs.remove(uri)
    vfs.get(file) shouldBe None
  }

  it should "advance the stamp on every update so an edit invalidates the cache" in {
    val vfs    = new VirtualFileSystem
    vfs.update(uri, "first")
    val first  = vfs.get(file).map(_.stamp)
    vfs.update(uri, "second")
    val second = vfs.get(file).map(_.stamp)
    second.exists(s => first.exists(s > _)) shouldBe true
  }

  it should "ignore non-file URIs (e.g. untitled buffers) rather than fail" in {
    val vfs = new VirtualFileSystem
    noException should be thrownBy vfs.update(URI.create("untitled:Untitled-1"), "x")
  }

  "VirtualFileStatProcessor" should "report the buffer stamp as the modification time of an overridden file" in {
    val vfs   = new VirtualFileSystem
    vfs.update(uri, "buffer content")
    val stamp = vfs.get(file).map(_.stamp).get
    runCompilerIO(VirtualFileStatProcessor(vfs).generate(FileStat.Key(file)) >> getFactOrAbort(FileStat.Key(file)))
      .asserting(_ shouldBe Right(FileStat(file, Some(Instant.ofEpochMilli(stamp)))))
  }

  it should "register nothing for a file without an override" in {
    runCompilerIO(VirtualFileStatProcessor(new VirtualFileSystem).generate(FileStat.Key(file)) >> getFactOrAbort(FileStat.Key(file)))
      .asserting(_ shouldBe Left(Chain.empty))
  }

  "VirtualFileContentReader" should "serve buffer content in place of the on-disk file when overridden" in {
    val vfs = new VirtualFileSystem
    vfs.update(uri, "buffer content")
    runCompilerIO {
      VirtualFileStatProcessor(vfs).generate(FileStat.Key(file)) >>
        VirtualFileContentReader(vfs).generate(FileContent.Key(file)) >>
        getFactOrAbort(FileContent.Key(file))
    }.asserting(_ shouldBe Right(FileContent(file, "buffer content")))
  }

  it should "register nothing for a file without an override" in {
    runCompilerIO(VirtualFileContentReader(new VirtualFileSystem).generate(FileContent.Key(file)) >> getFactOrAbort(FileContent.Key(file)))
      .asserting(_ shouldBe Left(Chain.empty))
  }

  it should "depend on the file stat, so it produces nothing until the stat is available" in {
    val vfs = new VirtualFileSystem
    vfs.update(uri, "buffer content")
    runCompilerIO(VirtualFileContentReader(vfs).generate(FileContent.Key(file)) >> getFactOrAbort(FileContent.Key(file)))
      .asserting(_ shouldBe Left(Chain.empty))
  }

  /** Run a CompilerIO against an in-memory process that stores registered facts, returning errors or the result. */
  private def runCompilerIO[T](value: CompilerIO[T]): IO[Either[Chain[CompilerError], T]] =
    value.run(new VirtualFileSystemTest.InMemoryProcess()).run(Chain.empty).value.map(_.map(_._2))
}

object VirtualFileSystemTest {
  private class InMemoryProcess extends CompilationProcess {
    private var facts: Map[CompilerFactKey[?], CompilerFact] = Map.empty

    override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
        key: K,
        ancestors: List[CompilerFactKey[?]]
    ): IO[Option[V]] =
      IO(facts.get(key).map(_.asInstanceOf[V]))

    override def registerFact(value: CompilerFact): IO[Unit] = IO { facts = facts.updated(value.key(), value) }
  }
}
