---
name: eliot-apidoc
description: Use when writing or editing `/** ... */` documentation comments in Eliot `.els` source, so they render correctly and read professionally in the apidoc site. Covers where comments attach, the exact Markdown subset supported, what is auto-extracted (so you never restate it), cross-referencing, and doc-writing best practices.
---

# Writing Eliot apidoc

The apidoc backend (`apidoc` Mill module) turns `/** ... */` comments into a static HTML site. Preview your output:

    ./mill apidoc.run apidoc <yourSourcePath> -o out-docs   # site in out-docs/apidoc/index.html

**Document each name exactly once, on the declaration in its *lowest* layer** — `lang` before `stdlib` before `compiler` before `jvm`. That is where the name is introduced (usually its abstract base declaration; for a handful of fundamentals like `Function`/`apply` it is the `lang` copy). Every higher layer that merely *repeats* the name — a compiler-internal duplicate, or a concrete platform body / `data` — must carry **no** doc comment. The generator renders the lowest layer's doc under the auto-generated abstract signature and shows the other layers as badges/definitions automatically. A doc comment on a higher layer is **ignored and reported as a build warning** (`Doc comment on 'X' in layer 'jvm' is ignored; 'X' is already documented in layer 'stdlib'.`), so keep every doc single-sourced and put anything platform-specific into that one lowest-layer doc rather than a second copy.

## Syntax & where a comment attaches

- Only `/** ... */` is a doc comment. `//` and `/* ... */` are ignored; `/**/` is an empty block, not a doc.
- It attaches to the **nearest declaration that follows it**. Put it immediately before the declaration — above any `infix`/`prefix`/`postfix`/`private`/`opaque` modifiers (it documents the whole declaration). If several comments precede one declaration, the closest wins; a trailing comment with nothing after it is dropped.
- Document each `def`, `type`, `data`, `ability`, and each **ability method** with its own comment.
- **Not yet rendered** (don't rely on these): module-level docs (a top-of-file comment attaches to the *first declaration*, not the module — so it shows under that declaration); per-`implement` docs (document the `ability` and the type instead); per-constructor / per-field docs (document at the `data` level).
- Leading-star margins are stripped Scaladoc-style — align each continuation line with a space, a `*`, and a space, then indent code under it normally.

## What is auto-extracted — never restate it

The full signature is generated from the code and shown above your prose, syntax-highlighted. Do **not** repeat any of it:

- name, generic parameters (with bounds, `auto`, and `~` constraints), value parameters **and their types**, return type **including effect rows** like `{Throw[String]} A`, fixity/precedence, `private`/`opaque`, and `data` constructors;
- which platforms implement the name (shown as `abstract` / `implemented on: jvm, compiler` badges) and an ability's implementors (gathered workspace-wide).

So there is no `@param`/`@return`/`@tparam`: describe *meaning*, referencing a parameter by name in backticks (e.g. `` `step` ``) only when it adds something the type doesn't.

## Markdown subset

Supported: paragraphs (blank-line separated; consecutive lines join), `` `inline code` ``, `**bold**`, `*italic*`, `[text](url)` links, `-`/`*` unordered lists, ATX headings `#`..`######` (rendered h4–h6), and fenced code blocks. A code fence tagged `eliot` **or untagged** is syntax-highlighted; any other tag is shown plain. HTML is escaped, so `<`, `>`, `&` are safe to type.

Not supported (avoid): ordered lists, nested lists, tables, block quotes, images, setext headings, raw HTML, backtick-escaping inside code spans.

The example below is shown indented so the literal comment markup is visible; in real source the body uses a fenced `eliot` block for the example:

    /**
     * Run `step` forever; the one effect that may reach `main` undischarged.
     *
     * ```eliot
     * def serve: {Inf, Console} Unit = forever(printLine("tick"))
     * def main: IO[Unit] = serve
     * ```
     */
    def forever(step: F[Unit]): F[Unit]

## Cross-referencing

Auto-linking is not implemented yet. Refer to other names with **inline code** — `` `Option` ``, `` `fold` `` — which reads well and is stable. Markdown links work, but item anchors are generated (`kind-name-hash`), so deep links are brittle; at most link to a module page, e.g. `[Option](eliot.lang.Option.html)`.

## Best practices

1. **Open with one self-contained summary sentence.** It is the lead and the most-read line; it must make sense alone, without the signature. e.g. *"An optional value: either present or absent."*
2. **Then describe the contract concisely** — what it computes/guarantees and *why it exists*, not the mechanics already visible in the signature.
3. **Give an example when it earns its place** — operators, combinators, effectful APIs, or any non-obvious behaviour. Make it realistic and self-explaining, not a toy one-liner (see **Writing examples** below). Skip examples for self-explanatory definitions.
4. **State partiality and use-site obligations.** Under Eliot's use-site verification a latent partiality surfaces at the *caller*; document failure conditions and bound/coercion requirements at the definition so the user is warned where they read it.
5. **Document the abstract contract platform-neutrally.** The base doc is what the merge shows; mention a platform only when its representation/behaviour is user-visible.
6. **Be concise and complete.** State the non-obvious, omit the obvious; do not restate types, parameter lists, effect rows, or which platforms implement it.

## Writing examples

Examples should read like real code a user would write, not signature restatements. The goal is a small
but *non-trivial* scenario that shows the definition in use, carrying its own explanation.

- **Multi-line and realistic.** Prefer a believable scenario — assembling a DB connection from settings,
  building an endpoint — over a one-liner that just echoes the signature. Show the definition *in the
  context a reader would actually meet it*.
- **Explain with comments, inside the code.** Put the explanation where the reader's eye already is:
  a leading `//` line above a `def` stating what it does or its contract, and trailing `//` notes on
  individual lines (a value's type, why a step short-circuits). Only spill into prose *outside* the
  block when an explanation is too long to sit inline — the block should be understandable on its own.
- **Block syntax for multiple statements.** When the body has several `val` bindings and a final
  result, use a `{ ... }` block; keep a single expression inline.
- **Use `=>`, never `Function`.** Write function types as `A => B` (right-associative, so
  `A => B => C` is `A => (B => C)`) and lambdas as `x -> body`. Do not write `Function[A, B]` in an example.
- **Prefer the dot / infix operator for an OO reading.** Where a function chains on a subject, write
  `subject.f(a).g(b)` or the infix `value orElse default`, not the inside-out `g(b, f(a, subject))` —
  it reads in run order and is the idiomatic surface (the subject is the *last* parameter).
- **Scaffold with signature-only stubs.** To keep an example self-contained, declare the supporting API
  as a body-less signature and then show the interesting function that uses it — the reader sees a
  complete, compiling-looking picture without unrelated bodies cluttering it.

Shown indented so the literal comment markup is visible; in real source the example is a fenced `eliot` block:

    /**
     * Run an `{Abort}` computation, supplying `fallback` if it short-circuits.
     *
     * ```eliot
     * // Get a setting as a String for a key. Aborts if the setting does not exist.
     * def setting(key: String): {Abort} String
     *
     * // Assemble a connection from settings. If any setting aborts, the whole
     * // function aborts before opening a connection.
     * def dbConnection: {Abort} Connection = {
     *   val host = setting("host")   // a String — past the possible abort
     *   val port = setting("port")   // likewise
     *   openConnection(host, port)
     * }
     *
     * // Always returns a String: the abort is absorbed by the fallback.
     * def portWithDefault: String = setting("port") orElse "8080"
     * ```
     */
    def orElse[G[_] ~ Effect, A](computation: AbortCarrier[G, A], fallback: A): G[A]

## Template

Fill in and place directly above the declaration (the `<...>` notes are placeholders):

    /**
     * <One-sentence summary that stands on its own.>
     *
     * <A short paragraph on behaviour, guarantees, and intent. Reference related
     * names like `fold` or `Option` in backticks. Note any way it can fail at a
     * use site.>
     *
     * ```eliot
     * <a realistic, multi-line example with inline `//` comments — see "Writing examples"; only if it helps>
     * ```
     */
    def name[...](...): ...

## Checklist

- Comment is `/** */`, directly above the declaration (above its modifiers).
- First line is a complete summary sentence.
- No restated signature/params/return/effects/platform info.
- Examples (if any) are in fenced `eliot` blocks: multi-line and realistic, self-explaining via inline `//` comments, block syntax for multiple statements, `=>`/dot-operator idiom (never `Function[...]`).
- Failure modes / use-site requirements noted where relevant.
- Previewed with `./mill apidoc.run apidoc ... -o out-docs`.
