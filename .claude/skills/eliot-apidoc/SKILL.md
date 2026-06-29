---
name: eliot-apidoc
description: Use when writing or editing `/** ... */` documentation comments in Eliot `.els` source, so they render correctly and read professionally in the apidoc site. Covers where comments attach, the exact Markdown subset supported, what is auto-extracted (so you never restate it), cross-referencing, and doc-writing best practices.
---

# Writing Eliot apidoc

The apidoc backend (`apidoc` Mill module) turns `/** ... */` comments into a static HTML site. Preview your output:

```bash
./mill apidoc.run apidoc <yourSourcePath> -o out-docs   # site in out-docs/apidoc/index.html
```

Write the canonical documentation on the **abstract declaration** in the base layer (`lang`/`stdlib`). The merge prefers the abstract signature *and* the abstract doc, and shows the platform implementations as badges/definitions automatically — so a `jvm`/`compiler` re-definition needs a doc only if its behaviour has user-relevant specifics.

## Syntax & where a comment attaches

- Only `/** ... */` is a doc comment. `//` and `/* ... */` are ignored; `/**/` is an empty block, not a doc.
- It attaches to the **nearest declaration that follows it**. Put it immediately before the declaration — *above* any `infix`/`prefix`/`postfix`/`private`/`opaque` modifiers (it documents the whole declaration). If several comments precede one declaration, the closest wins; a trailing comment with nothing after it is dropped.
- Document each `def`, `type`, `data`, `ability`, and each **ability method** with its own comment.
- **Not yet rendered** (don't rely on these): module-level docs (a top-of-file comment attaches to the *first declaration*, not the module — so it shows under that declaration); per-`implement` docs (document the `ability` and the type instead); per-constructor / per-field docs (document at the `data` level).
- Leading `*` margins are stripped Scaladoc-style — align continuation lines with ` * ` and indent code under it normally.

## What is auto-extracted — never restate it

The full signature is generated from the code and shown above your prose, syntax-highlighted. Do **not** repeat any of it:

- name, generic parameters (with bounds, `auto`, and `~` constraints), value parameters **and their types**, return type **including effect rows** like `{Throw[String]} A`, fixity/precedence, `private`/`opaque`, and `data` constructors;
- which platforms implement the name (shown as `abstract` / `implemented on: jvm, compiler` badges) and an ability's implementors (gathered workspace-wide).

So there is no `@param`/`@return`/`@tparam`: describe *meaning*, referencing a parameter by name in backticks (e.g. `` `step` ``) only when it adds something the type doesn't.

## Markdown subset

Supported: paragraphs (blank-line separated; consecutive lines join), `` `inline code` ``, `**bold**`, `*italic*`, `[text](url)` links, `-`/`*` unordered lists, ATX headings `#`..`######` (rendered h4–h6), and fenced code blocks. A code fence tagged `eliot` **or untagged** is syntax-highlighted; any other tag is shown plain. HTML is escaped, so `<`, `>`, `&` are safe to type.

Not supported (avoid): ordered lists, nested lists, tables, block quotes, images, setext headings, raw HTML, backtick-escaping inside code spans.

```eliot
/**
 * Run `step` forever; the one effect that may reach `main` undischarged.
 *
 * ```eliot
 * def main: {Inf} IO[Unit] = forever(println("tick"))
 * ```
 */
def forever(step: F[Unit]): F[Unit]
```

## Cross-referencing

Auto-linking is not implemented yet. Refer to other names with **inline code** — `` `Option` ``, `` `fold` `` — which reads well and is stable. Markdown links work, but item anchors are generated (`kind-name-hash`), so deep links are brittle; at most link to a module page (`[Option](eliot.lang.Option.html)`).

## Best practices

1. **Open with one self-contained summary sentence.** It is the lead and the most-read line; it must make sense alone, without the signature. e.g. *"An optional value: either present or absent."*
2. **Then describe the contract concisely** — what it computes/guarantees and *why it exists*, not the mechanics already visible in the signature.
3. **Give an example when it earns its place** — operators, combinators, effectful APIs, or any non-obvious behaviour. Use a runnable-looking ```eliot block; keep it minimal. Skip examples for self-explanatory definitions.
4. **State partiality and use-site obligations.** Under Eliot's use-site verification a latent partiality surfaces at the *caller*; document failure conditions and bound/coercion requirements at the definition so the user is warned where they read it.
5. **Document the abstract contract platform-neutrally.** The base doc is what the merge shows; mention a platform only when its representation/behaviour is user-visible.
6. **Be concise and complete.** State the non-obvious, omit the obvious; do not restate types, parameter lists, effect rows, or which platforms implement it.

## Template

```eliot
/**
 * <One-sentence summary that stands on its own.>
 *
 * <A short paragraph on behaviour, guarantees, and intent. Reference related
 * names like `fold` or `Option` in backticks. Note any way it can fail at a
 * use site.>
 *
 * ```eliot
 * <a small, illustrative example — only if it helps>
 * ```
 */
def name[...](...): ...
```

## Checklist

- Comment is `/** */`, directly above the declaration (above its modifiers).
- First line is a complete summary sentence.
- No restated signature/params/return/effects/platform info.
- Examples (if any) are in ```eliot fences and minimal.
- Failure modes / use-site requirements noted where relevant.
- Previewed with `./mill apidoc.run apidoc ... -o out-docs`.
