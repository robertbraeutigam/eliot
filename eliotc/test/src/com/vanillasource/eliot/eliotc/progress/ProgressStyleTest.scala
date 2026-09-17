package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.progress.ProgressLineWriterTest.sgr
import com.vanillasource.eliot.eliotc.progress.ProgressStyle.Colour
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalTime

class ProgressStyleTest extends AnyFlatSpec with Matchers {

  "the progress style" should "colour a terminal" in {
    ProgressStyle.from(Map("TERM" -> "xterm"), terminal = true, "UTF-8") shouldBe
      ProgressStyle(Colour.Ansi, unicode = true, timestamped = false)
  }

  it should "use 24-bit colour when the terminal says it has it" in {
    ProgressStyle.from(Map("COLORTERM" -> "truecolor"), terminal = true, "UTF-8").colour shouldBe Colour.TrueColour
  }

  it should "write a log when not writing to a terminal" in {
    ProgressStyle.from(Map("COLORTERM" -> "truecolor"), terminal = false, "UTF-8") shouldBe
      ProgressStyle(Colour.None, unicode = true, timestamped = true)
  }

  it should "write a log to a dumb terminal" in {
    ProgressStyle.from(Map("TERM" -> "dumb"), terminal = true, "UTF-8") shouldBe
      ProgressStyle(Colour.None, unicode = true, timestamped = true)
  }

  it should "leave colour out under NO_COLOR, and nothing else" in {
    ProgressStyle.from(Map("NO_COLOR" -> "1"), terminal = true, "UTF-8") shouldBe
      ProgressStyle(Colour.None, unicode = true, timestamped = false)
  }

  it should "ignore an empty NO_COLOR" in {
    ProgressStyle.from(Map("NO_COLOR" -> ""), terminal = true, "UTF-8").colour shouldBe Colour.Ansi
  }

  it should "not write Unicode in an encoding that is not UTF-8" in {
    ProgressStyle.from(Map.empty, terminal = true, "ANSI_X3.4-1968").unicode shouldBe false
  }

  it should "degrade the separator and the ellipsis to ASCII" in {
    ascii.separator + ascii.ellipsis shouldBe " - ..."
  }

  it should "paint in 24-bit colour from the design system" in {
    ProgressStyle(Colour.TrueColour, unicode = true, timestamped = false).pass("ok") shouldBe
      s"${27.toChar}[38;2;46;194;126mok${sgr(0)}"
  }

  it should "not paint an empty text" in {
    ProgressStyle(Colour.Ansi, unicode = true, timestamped = false).faint("") shouldBe ""
  }

  it should "stamp a log's lines with the time of day" in {
    ProgressStyle(Colour.None, unicode = true, timestamped = true).stamped("ok", LocalTime.of(11, 42, 3)) shouldBe
      "11:42:03 ok"
  }

  it should "not stamp a terminal's lines" in {
    ascii.stamped("ok", LocalTime.of(11, 42, 3)) shouldBe "ok"
  }

  private val ascii = ProgressStyle(Colour.None, unicode = false, timestamped = false)
}
