package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.SourceAST
import com.vanillasource.eliot.eliotc.token.Tokenizer

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The gate on the **staged v6 tree** (`.v6/`, effects v6 §10.1 step 8): every `.els` prepared for the flag day
  * tokenizes and parses with today's compiler.
  *
  * The directory is hidden because `SourceRootDiscovery` skips a dot-directory: were it visible, the LSP would take
  * `.v6/stdlib/eliot` and its siblings for layer roots beside the real ones and turn the whole workspace red.
  *
  * That is the whole of what can be checked before the flag day — `effect`, the named `implement` and `with` parse but
  * are rejected at core, and the v6 signatures do not type-check against a v5 tree — but it is not nothing: it is what
  * keeps a tree nobody compiles from rotting into unparseable text while the compiler moves under it. The rest of the
  * gate is §10.2 F8, after the tree is landed.
  *
  * Delete this test with the staging directory at F7.
  */
class V6TreeParseTest extends ProcessorTest(new Tokenizer(), new ASTParser()) {
  import V6TreeParseTest.*

  "every staged v6 source" should "tokenize and parse" in {
    stagedSources.traverse(p => parseErrors(p).map(p -> _)).asserting(_.filter(_._2.nonEmpty) shouldBe empty)
  }

  it should "be a non-empty set of files" in {
    IO.pure(stagedSources.size).asserting(_ should be > 40)
  }

  private def parseErrors(source: Path): IO[Seq[String]] =
    IO.blocking(Files.readString(source))
      .flatMap(content => runGenerator(content, SourceAST.Key(file)))
      .map(_._1.map(_.message))
}

object V6TreeParseTest {
  private val repoRoot: Path =
    Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))

  private val stagedSources: Seq[Path] =
    Files.walk(repoRoot.resolve(".v6")).iterator().asScala.filter(_.toString.endsWith(".els")).toSeq.sorted
}
