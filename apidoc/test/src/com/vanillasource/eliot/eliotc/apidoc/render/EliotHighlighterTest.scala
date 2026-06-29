package com.vanillasource.eliot.eliotc.apidoc.render

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EliotHighlighterTest extends AnyFlatSpec with Matchers {
  import EliotHighlighter.highlight

  "eliot highlighter" should "classify a hard keyword" in {
    highlight("def") shouldBe """<span class="kw">def</span>"""
  }

  it should "classify an upper-case identifier as a type" in {
    highlight("Int") shouldBe """<span class="ty">Int</span>"""
  }

  it should "classify a lower-case identifier as a function/value" in {
    highlight("foo") shouldBe """<span class="fn">foo</span>"""
  }

  it should "classify an integer literal" in {
    highlight("42") shouldBe """<span class="num">42</span>"""
  }

  it should "glue a leading minus into a negative integer literal" in {
    highlight("-128") shouldBe """<span class="num">-128</span>"""
  }

  it should "classify a string literal with escaped quotes" in {
    highlight("\"hi\"") shouldBe """<span class="str">&quot;hi&quot;</span>"""
  }

  it should "classify a line comment" in {
    highlight("// note") shouldBe """<span class="cm">// note</span>"""
  }

  it should "classify an operator run and escape angle brackets" in {
    highlight("a -> b") shouldBe """<span class="fn">a</span> <span class="op">-&gt;</span> <span class="fn">b</span>"""
  }

  it should "leave brackets and commas as bare (escaped) punctuation" in {
    highlight("Int[A, B]") shouldBe
      """<span class="ty">Int</span>[<span class="ty">A</span>, <span class="ty">B</span>]"""
  }
}
