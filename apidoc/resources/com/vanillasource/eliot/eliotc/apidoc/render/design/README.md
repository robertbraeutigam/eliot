# Vendored Eliot Design System tokens

These `.css` files are **verbatim copies** of the token layer of the *Eliot Design System*
project on `claude.ai/design` (project id `d2fd52f1-987a-42a0-8de5-a3355e7787ab`). They are the
single source of the apidoc site's brand: colors, typography, spacing, effects, and base element
defaults.

`HtmlSite` concatenates them (in the order below — the same order as the design system's own
`styles.css`) ahead of its component CSS, then references only the **semantic tokens** they define
(`--bg`, `--surface`, `--text`, `--text-muted`, `--border`, `--accent`, `--accent-wash`,
`--radius-*`, `--font-display/body/code`, `--syn-*`, …). So a brand change made in Claude Design
flows into the generated docs by **re-syncing these files** — no Scala edits.

```
fonts.css       JetBrains Mono + Inter @font-face (Fontsource CDN; system fallbacks)
colors.css      base palette + semantic aliases (dark default, .on-paper = light docs theme)
typography.css  families, weights, type scale, line-heights, tracking
spacing.css     4px spacing scale + layout sizes
effects.css     radii, borders, shadows, motion, focus ring, cursor cadence
base.css        global element defaults, ::selection, focus-visible, .eliot-cursor, eliot-blink
```

## How the docs use them

The generated `<body>` carries `class="on-paper"`, which flips the semantic tokens to the brand's
light long-form-docs surface. Code wells (`pre.sig`, `.doc pre.code`) intentionally use the
*fixed* dark base-palette tokens (`--ink`, `--paper`, `--line`, `--syn-*`) so code stays dark and
is the visual hero, exactly as the design system's `ui_kits/docs` kit prescribes.

## Re-syncing

Ask Claude Code to "re-sync the apidoc design tokens from Claude Design". It will read each token
file from the project above (via the design-sync integration) and overwrite the copies here. Keep
them verbatim — apidoc-specific styling belongs in `HtmlSite`, never edited into these files.
