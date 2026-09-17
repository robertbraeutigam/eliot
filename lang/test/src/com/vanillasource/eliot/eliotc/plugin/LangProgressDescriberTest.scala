package com.vanillasource.eliot.eliotc.plugin

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.progress.ProgressActivity
import com.vanillasource.eliot.eliotc.source.file.FileContent
import com.vanillasource.eliot.eliotc.source.stat.FileStat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class LangProgressDescriberTest extends AnyFlatSpec with Matchers {
  private val stringLength =
    ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("length", Qualifier.Default))

  "the lang describer" should "name a value's check by its module" in {
    LangProgressDescriber.describe(MonomorphicValue.Key(stringLength, Seq.empty)) shouldBe
      Some(ProgressActivity("checking", "eliot.lang.String"))
  }

  it should "name a source file's content as an input, relative to the working directory" in {
    LangProgressDescriber.describe(FileContent.Key(File("src/HelloWorld.els").getAbsoluteFile)) shouldBe
      Some(ProgressActivity("reading", "src/HelloWorld.els", input = true))
  }

  it should "not name a file's stat as an input, since a recently written file's stat never compares equal" in {
    LangProgressDescriber.describe(FileStat.Key(File("src/HelloWorld.els"))).map(_.input) shouldBe Some(false)
  }
}
