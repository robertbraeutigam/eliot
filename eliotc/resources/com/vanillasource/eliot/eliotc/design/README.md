# Vendored Eliot Design System tokens (shared)

These `.css` files are **verbatim copies** of the token layer of the *Eliot Design System*
project on `claude.ai/design` (project id `d2fd52f1-987a-42a0-8de5-a3355e7787ab`). They are the
single source of the brand — colors, typography, spacing, effects, and base element defaults —
for **every** HTML generator in the compiler.

They live in the `eliotc` core module so they sit on the classpath of every module that depends on
it. `com.vanillasource.eliot.eliotc.design.DesignTokens` reads them (by absolute classpath path) and
exposes `tokensCss` (all six concatenated, in the order below — the same order as the design
system's own `styles.css`) and `favicon`. Each generator prepends `DesignTokens.tokensCss` to its
own component CSS and references only the **semantic tokens** they define (`--bg`, `--surface`,
`--text`, `--text-muted`, `--border`, `--accent`, `--accent-wash`, `--radius-*`,
`--font-display/body/code`, `--syn-*`, …). So a brand change made in Claude Design flows into every
generated page by **re-syncing these files** — no Scala edits.

```
fonts.css       JetBrains Mono + Inter @font-face (Fontsource CDN; system fallbacks)
colors.css      base palette + semantic aliases (dark default, .on-paper = light docs theme)
typography.css  families, weights, type scale, line-heights, tracking
spacing.css     4px spacing scale + layout sizes
effects.css     radii, borders, shadows, motion, focus ring, cursor cadence
base.css        global element defaults, ::selection, focus-visible, .eliot-cursor, eliot-blink
```

## Consumers

- **apidoc** (`apidoc/.../render/HtmlSite`) — the documentation site. Adds `class="on-paper"` to
  flip the semantic tokens to the brand's light long-form-docs surface; code wells stay dark.
- **fact-flow visualizer** (`eliotc/.../visualization/FactVisualizationTracker`) — the interactive
  Cytoscape graph. Uses the **default dark theme** (an app surface, not docs), with green "signal"
  edges on an ink canvas.

Both reference the *fixed* dark base-palette tokens (`--ink`, `--paper`, `--line`, `--syn-*`) where
they need a permanently-dark surface (apidoc's code wells; the visualizer's whole canvas).

## Re-syncing

Ask Claude Code to "re-sync the design tokens from Claude Design". It will read each token file from
the project above (via the design-sync integration) and overwrite the copies here. Keep them
verbatim — generator-specific styling belongs in each generator, never edited into these files.
