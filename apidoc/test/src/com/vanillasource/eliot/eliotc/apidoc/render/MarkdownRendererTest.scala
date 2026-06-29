package com.vanillasource.eliot.eliotc.apidoc.render

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownRendererTest extends AnyFlatSpec with Matchers {
  import MarkdownRenderer.render

  "markdown renderer" should "wrap a plain line in a paragraph" in {
    render("hello world") shouldBe "<p>hello world</p>\n"
  }

  it should "render an inline code span" in {
    render("call `foo` now") shouldBe "<p>call <code>foo</code> now</p>\n"
  }

  it should "render bold and italic" in {
    render("**b** and *i*") shouldBe "<p><strong>b</strong> and <em>i</em></p>\n"
  }

  it should "render a link" in {
    render("see [here](Int.html)") shouldBe """<p>see <a href="Int.html">here</a></p>"""+ "\n"
  }

  it should "not apply inline formatting inside a code span" in {
    render("`a*b*c`") shouldBe "<p><code>a*b*c</code></p>\n"
  }

  it should "escape HTML in prose" in {
    render("a < b & c") shouldBe "<p>a &lt; b &amp; c</p>\n"
  }

  it should "render an ATX heading starting at h4" in {
    render("# Title") shouldBe "<h4>Title</h4>\n"
  }

  it should "render an unordered list" in {
    render("- a\n- b") shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n</ul>\n"
  }

  it should "highlight an eliot fenced code block" in {
    render("```eliot\ndef x: Unit\n```") should include("""<pre class="code"><code><span class="kw">def</span>""")
  }

  it should "join consecutive prose lines into one paragraph" in {
    render("line one\nline two") shouldBe "<p>line one line two</p>\n"
  }
}
