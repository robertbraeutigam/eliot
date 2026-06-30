package com.vanillasource.eliot.eliotc.design

import java.nio.charset.StandardCharsets

/** The Eliot brand design system, shared by every HTML generator in the compiler (the apidoc documentation site, the
  * fact-flow visualizer). The token files under `design/` are vendored verbatim from the Eliot Design System project on
  * claude.ai/design — see `design/README.md` for provenance and how to re-sync.
  *
  * Each generator prepends [[tokensCss]] to its own component CSS and then references only the semantic tokens it
  * defines (`--bg`/`--surface`/`--text`/`--accent`/`--radius-*`/`--font-*`/`--syn-*`, …). A brand change therefore
  * flows into every generated page by re-syncing the token files, with no Scala edits.
  */
object DesignTokens {

  /** The brand `e` + green-cursor mark, inlined as a data URI so generated pages stay self-contained. */
  val favicon: String =
    "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA2NCA2NCIgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByb2xlPSJpbWciIGFyaWEtbGFiZWw9ImVsaW90Ij48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNjQiIGhlaWdodD0iNjQiIHJ4PSIxNC4xIiBmaWxsPSIjMEYxNDE5Ij48L3JlY3Q+PGcgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoMTUuMzgsNDQuODApIHNjYWxlKDAuMDM1MDY4NDkzMTUwNjg0OTM2LC0wLjAzNTA2ODQ5MzE1MDY4NDkzNikiPjxwYXRoIGQ9Ik0zMDAgLTEwUTIwMSAtMTAgMTQwLjUgNDkuMFE4MCAxMDggODAgMjEwVjM0MFE4MCA0NDIgMTQwLjUgNTAxLjBRMjAxIDU2MCAzMDAgNTYwUTM2NiA1NjAgNDE1LjUgNTMzLjVRNDY1IDUwNyA0OTIuNSA0NTkuNVE1MjAgNDEyIDUyMCAzNDlWMjUwSDE3N1YyMDJRMTc3IDE0MiAyMTAuMCAxMDguMFEyNDMgNzQgMzAwIDc0UTM0OCA3NCAzNzkuMCA5Mi4wUTQxMCAxMTAgNDE2IDE0Mkg1MTVRNTA1IDcyIDQ0Ni4wIDMxLjBRMzg3IC0xMCAzMDAgLTEwWk0xNzcgMzQ5VjMyM0g0MjNWMzQ5UTQyMyA0MTIgMzkxLjAgNDQ2LjBRMzU5IDQ4MCAzMDAgNDgwUTI0MSA0ODAgMjA5LjAgNDQ2LjBRMTc3IDQxMiAxNzcgMzQ5WiIgZmlsbD0iI0VERUFFMyI+PC9wYXRoPjwvZz48cmVjdCB4PSI0MC42OSIgeT0iMTkuMjAiIHdpZHRoPSI1LjEyIiBoZWlnaHQ9IjI1LjYwIiBmaWxsPSIjMkVDMjdFIj48L3JlY3Q+PC9zdmc+"

  /** The design system's token layer (fonts, colors, typography, spacing, effects, base) concatenated in the same order
    * as the design system's own `styles.css`. Prepend this to a page's component CSS.
    */
  val tokensCss: String =
    Seq("fonts.css", "colors.css", "typography.css", "spacing.css", "effects.css", "base.css")
      .map(name => s"/* ---- design/$name ---- */\n" + resource(name))
      .mkString("\n")

  /** Reads a vendored design resource from the classpath (absolute path so any caller resolves the same files). Missing
    * resources fail loudly rather than degrading silently — a page must never render without its brand tokens.
    */
  private def resource(name: String): String = {
    val path   = s"/com/vanillasource/eliot/eliotc/design/$name"
    val stream = Option(getClass.getResourceAsStream(path))
      .getOrElse(throw new IllegalStateException(s"Missing vendored design resource: $path"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }
}
