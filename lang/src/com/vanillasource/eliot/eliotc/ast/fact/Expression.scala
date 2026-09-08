package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.pos.Position
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Expression

object Expression {
  /** @param genericArguments
    *   The `[...]` type arguments. `None` means no brackets were written; `Some(Seq())` means an explicit empty `[]`
    *   (which forces the name into the Type namespace even in value position, the `[]` analogue of value-level `()`);
    *   `Some(xs)` means `[xs]`.
    */
  case class FunctionApplication(
      moduleName: Option[Sourced[String]],
      functionName: Sourced[String],
      genericArguments: Option[Seq[Sourced[Expression]]],
      arguments: Seq[Sourced[Expression]]
  ) extends Expression
  case class FunctionLiteral(parameters: Seq[LambdaParameterDefinition], body: Sourced[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[String])                                        extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])                                          extends Expression
  case class FlatExpression(parts: Seq[Sourced[Expression]])                                        extends Expression
  case class MatchExpression(scrutinee: Sourced[Expression], cases: Seq[MatchCase])                 extends Expression

  /** A `{ … }` block: a sequence of statement/binding lines ending in a result expression. Over-separated at parse time
    * (one [[BlockLine]] per source line); adjacent lines are re-joined by fixity and the whole block is lowered to a
    * tower of immediately-applied lambdas by `block.processor.BlockDesugaringProcessor`, so it never survives past
    * resolution.
    */
  case class BlockExpression(lines: Seq[BlockLine]) extends Expression

  /** One source line of a [[BlockExpression]]: an optional `val name [: type] =` binder (reusing the lambda-parameter
    * shape) and the line's flat expression. A line with no binder is a bare statement; the last line of a block (which
    * must have no binder) is the block's result.
    */
  case class BlockLine(binder: Option[LambdaParameterDefinition], expression: Sourced[Expression])

  /** The effect-row sugar `{ E1, E2, … } A` written in a type position, in two forms distinguished by `tail`:
    *
    * **Open row** (`tail == None`) — `{Console} A`: the unordered set of `effects` the computation *performs*, over a
    * caller-chosen carrier. Pure, type-information-free — they never survive past the core processor, where
    * [[core.processor.EffectSugarDesugarer]] collapses every `{…} A` occurrence in a signature to `F[A]` under one
    * shared inferable carrier `F[_]` (each becoming an `F ~ Ei` constraint).
    *
    * **Pinned row** (`tail == Some(base)`) — `{Throw[E] | G} A`: a *concrete type*, the canonical carrier stack
    * realizing exactly these effects over the base `tail`. Entries are **ordered** (leftmost = outermost = discharged
    * first) and each must be backed by a canonical carrier named by convention `<Ability>Carrier`, colocated with the
    * ability: the desugarer rewrites `{Throw[E], State[S] | Id} A` to `ThrowCarrier[E, StateCarrier[S, Id], A]`. No
    * generic parameter is introduced — a pinned row is just type-application spelled in effect vocabulary.
    *
    * Discharge is no longer annotated on the row: a discharger's effect vanishes structurally at the monomorphize-phase
    * residual check (it lands on an inner transformer carrier, absent from the caller's ambient), so there is no
    * negative `-E` member.
    */
  case class EffectfulType(
      effects: Seq[UnresolvedAbilityConstraint[Sourced[Expression]]],
      resultType: Sourced[Expression],
      tail: Option[Sourced[Expression]]
  ) extends Expression

  /** `subject with implementation` — effects v6's binding of a named implementation to its subject
    * (`docs/effects.md` §9.3). Infix, subject-first, at the loosest precedence and left-associative: `c with a with b`
    * is `(c with a) with b`, and `xs.sort.render with reverseOrd` applies to the whole chain. The same node serves both
    * positions — an expression (`greeting("Bob") with recordingConsole`) and a parameter's or field's type
    * (`body: {Console} Unit with mockConsole`); a `with` on a def's own return type or inside a row is a parse error by
    * construction, since neither parser admits it. `implementation` is a bare, optionally module-qualified name.
    *
    * Landed dark (§10.1 step 4): parsed here, rejected at core as not supported yet.
    */
  case class WithBinding(subject: Sourced[Expression], implementation: Sourced[Expression]) extends Expression

  case class MatchCase(pattern: Sourced[Pattern], body: Sourced[Expression])

  /** A reference to the boolean literal `true` (`eliot.lang.Bool::true`), the default ability-implementation guard
    * (ability-guards §2.3): a synthesized `implement`/`data` marker's return-type slot carries its guard, and an
    * unguarded implementation gets this `true`. It is written *module-qualified* rather than as the bare name `true`
    * so it resolves in every module without that module importing `Bool` (the resolver's `module::name` path looks the
    * value up by FQN, bypassing import scope) — mirroring real builds, where `Bool` is always on the layer path.
    */
  def trueReference(at: Sourced[?]): Sourced[Expression] =
    at.as(
      FunctionApplication(
        Some(at.as(WellKnownTypes.boolTrueFQN.moduleName.show)),
        at.as(WellKnownTypes.boolTrueFQN.name.name),
        None,
        Seq.empty
      )
    )

  /** Render this expression back to concrete source-like syntax. A plain diagnostic/apidoc renderer (never the
    * `cats.Show` typeclass, so generic `Show`-constrained code cannot pick an internal AST node up by accident).
    */
  extension (self: Expression)
    def render: String = self match {
      case IntegerLiteral(Sourced(_, _, value))                                                 => value
      case StringLiteral(Sourced(_, _, value))                                                  => value
      case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), ga, ns @ _ :: _) =>
        val gaStr = ga.fold("")(_.map(_.value.render).mkString("[", ", ", "]"))
        s"$module::$fn$gaStr(${ns.map(_.value.render).mkString(", ")})"
      case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), ga, _)           =>
        val gaStr = ga.fold("")(_.map(_.value.render).mkString("[", ", ", "]"))
        s"$module::$fn$gaStr"
      case FunctionApplication(None, Sourced(_, _, value), ga, ns @ _ :: _)                     =>
        val gaStr = ga.fold("")(_.map(_.value.render).mkString("[", ", ", "]"))
        s"$value$gaStr(${ns.map(_.value.render).mkString(", ")})"
      case FunctionApplication(None, Sourced(_, _, value), ga, _)                               =>
        val gaStr = ga.fold("")(_.map(_.value.render).mkString("[", ", ", "]"))
        s"$value$gaStr"
      case FunctionLiteral(parameters, body)                                                    =>
        parameters.map(_.render).mkString("(", ", ", ")") + " -> " + body.show
      case FlatExpression(parts)                                                                => parts.map(_.value.render).mkString(" ")
      case MatchExpression(scrutinee, cases)                                                    =>
        s"${scrutinee.value.render} match { ${cases.map(c => s"case ${c.pattern.value.render} -> ${c.body.value.render}").mkString(" ")} }"
      case EffectfulType(effects, resultType, tail)                                             =>
        val members = effects.map(renderAbilityConstraint)
        val tailStr = tail.fold("")(t => s" | ${t.value.render}")
        s"{${members.mkString(", ")}$tailStr} ${resultType.value.render}"
      case BlockExpression(lines)                                                               =>
        lines.map(renderBlockLine).mkString("{ ", "; ", " }")
      case WithBinding(subject, implementation)                                                 =>
        s"${subject.value.render} with ${implementation.value.render}"
    }

  private def renderBlockLine(line: BlockLine): String =
    line.binder.map(b => s"val ${b.render} = ").getOrElse("") + line.expression.value.render

  private def renderAbilityConstraint(ac: UnresolvedAbilityConstraint[Sourced[Expression]]): String =
    ac.abilityName.value +
      (if (ac.typeArgs.isEmpty) "" else ac.typeArgs.map(_.value.render).mkString("[", ", ", "]"))

  // Shared sub-parsers, all using fullParser for inner expression positions

  private lazy val moduleParser: Parser[Sourced[Token], Sourced[String]] =
    for {
      moduleParts <- acceptIf(isPackageSegment, "module name").atLeastOnceSeparatedBy(symbol("."))
    } yield {
      val moduleString = moduleParts.map(_.value.content).mkString(".")
      val outline      = Sourced.outline(moduleParts)
      outline.as(moduleString)
    }

  private lazy val integerLiteralParser: Parser[Sourced[Token], Expression] = for {
    lit <- acceptIf(isIntegerLiteral, "integer literal")
  } yield IntegerLiteral(lit.map(_.content))

  private lazy val stringLiteralParser: Parser[Sourced[Token], Expression] = for {
    lit <- acceptIf(isStringLiteral, "string literal")
  } yield StringLiteral(lit.map(_.content))

  private lazy val parenthesizedExprParser: Parser[Sourced[Token], Expression] =
    for {
      _ <- symbol("(")
      result <- fullParser
      _ <- symbol(")")
    } yield result

  private lazy val functionLiteralParser: Parser[Sourced[Token], Expression] = for {
    parameters <-
      bracketedCommaSeparatedItems("(", component[LambdaParameterDefinition], ")") or
        component[LambdaParameterDefinition].map(Seq(_))
    _          <- symbol("->")
    body       <- sourced(fullParser)
  } yield FunctionLiteral(parameters, body)

  private lazy val matchCaseParser: Parser[Sourced[Token], MatchCase] = for {
    _       <- keyword("case")
    pattern <- sourced(component[Pattern])
    _       <- symbol("->")
    body    <- sourced(fullParser)
  } yield MatchCase(pattern, body)

  private lazy val matchExpressionParser: Parser[Sourced[Token], Seq[MatchCase]] =
    matchCaseParser.atLeastOnce().between(keyword("match") *> symbol("{"), symbol("}"))

  /** Atoms for type and value positions: named references (with optional generic and *adjacent* value arguments),
    * parenthesized expressions, literals. Excludes unparenthesized lambdas and match expressions to avoid ambiguity in
    * type annotations.
    *
    * Uses the adjacency-sensitive [[adjacentCallParser]], so a value-argument list `(…)` attaches only when its `(` is
    * adjacent (no intervening whitespace) to the name: `f(x)` is the call, while `f (x)` leaves `(x)` a separate atom.
    * A non-infix `f (x)` still reduces to the application `f(x)` through the operator phase's operand currying; the
    * distinction only matters for an *infix* operator, where `a op (x)` must read as `op(a, x)` rather than `a(op(x))`
    * — this is what lets an infix operator take a parenthesized operand, e.g. `result catch (err -> fallback(err))`.
    */
  private lazy val typeAtom: Parser[Sourced[Token], Expression] =
    parenthesizedExprParser.atomic() or
      adjacentCallParser or
      integerLiteralParser or
      stringLiteralParser

  /** The `val name [: type] =` binder prefix of a block line, reusing the lambda-parameter shape for `name [: type]`.
    * Atomic so a non-`val` (statement) line backtracks cleanly to a binder-less parse.
    */
  private lazy val blockBinderParser: Parser[Sourced[Token], LambdaParameterDefinition] =
    (keyword("val") *> component[LambdaParameterDefinition] <* symbol("=")).atomic()

  /** One source line of a block: an optional binder then the line's atom run (line-bounded), optionally followed by a
    * trailing `match { … }` whose scrutinee is that atom run. Over-separation happens here: the run stops at every
    * newline, so each source line becomes one [[BlockLine]].
    */
  private lazy val blockLineParser: Parser[Sourced[Token], BlockLine] = for {
    binder     <- blockBinderParser.optional()
    atoms      <- lineBoundedAtoms(sourced(fullAtom))
    matchBlock <- matchExpressionParser.optional()
    bindings   <- withBindingsParser
  } yield {
    val flat = Sourced.outline(atoms).as(FlatExpression(atoms))
    val expression = matchBlock match {
      case Some(cases) =>
        val scrutinee = if (atoms.size == 1) atoms.head else flat
        Sourced.outline(atoms).as(MatchExpression(scrutinee, cases))
      case None        => flat
    }
    BlockLine(binder, applyWithBindings(expression, bindings))
  }

  private lazy val blockParser: Parser[Sourced[Token], Expression] =
    blockLineParser.anyTimes().between(symbol("{"), symbol("}")).map(BlockExpression.apply)

  /** Full atoms including lambdas and `{ … }` blocks. The block alternative is first and atomic: a leading `{` in value
    * position is always a block (the effect-set sugar `{…} A` lives only in type positions — see [[typeRunParser]] —
    * never here), and a non-`{` start backtracks to the lambda/type atoms.
    */
  private lazy val fullAtom: Parser[Sourced[Token], Expression] =
    blockParser.atomic() or functionLiteralParser.atomic() or typeAtom

  /** Full expression parser including lambdas and match expressions. */
  private lazy val fullParser: Parser[Sourced[Token], Expression] =
    for {
      parts      <- sourced(fullAtom).atLeastOnce()
      matchBlock <- matchExpressionParser.optional()
      bindings   <- withBindingsParser
    } yield {
      val subject = matchBlock match {
        case Some(cases) =>
          val scrutinee = if (parts.size == 1) parts.head else Sourced.outline(parts).as(FlatExpression(parts))
          Sourced.outline(parts).as(MatchExpression(scrutinee, cases))
        case None        => Sourced.outline(parts).as(FlatExpression(parts))
      }
      applyWithBindings(subject, bindings).value
    }

  /** The trailing `with name` chain of an expression or a parameter type — zero or more, read after everything else so
    * `with` sits at the loosest precedence (see [[WithBinding]]). Each name is a bare implementation reference: an
    * optionally module-qualified lower-case identifier, never applied.
    */
  private lazy val withBindingsParser: Parser[Sourced[Token], Seq[Sourced[Expression]]] =
    (keyword("with") *> sourced(implementationReferenceParser)).anyTimes()

  private lazy val implementationReferenceParser: Parser[Sourced[Token], Expression] = for {
    module <- (moduleParser <* symbol("::")).atomic().optional()
    name   <- acceptIfAll(isIdentifier, isLowerCase)("implementation name")
  } yield FunctionApplication(module, name.map(_.content), None, Seq.empty)

  /** Fold a `with` chain onto its subject left-associatively: `c with a with b` is `(c with a) with b`. */
  def applyWithBindings(subject: Sourced[Expression], bindings: Seq[Sourced[Expression]]): Sourced[Expression] =
    bindings.foldLeft(subject) { (acc, implementation) =>
      Sourced.outline(Seq(acc, implementation)).as(WithBinding(acc, implementation))
    }

  /** [[withBindingsParser]] for a parameter's or field's type position (`body: {Console} Unit with mockConsole`). */
  def typeWithBindings(typeExpression: Sourced[Expression]): Parser[Sourced[Token], Sourced[Expression]] =
    withBindingsParser.map(applyWithBindings(typeExpression, _))

  /** Parses the effect-row sugar `{ Eff (, Eff)* [| tail] } <type atom>`, e.g. `{Suspend} String`, `{State[Account],
    * Abort} A`, or the *pinned* form `{Throw[E] | Id} A` naming the base carrier after `|`. Each brace entry is an
    * ability reference (the same shape as a `~` ability constraint). The row covers exactly the one type atom that
    * follows, and is itself a type-run atom ([[typeRunAtom]]), so it also reads nested in a run — most usefully in an
    * arrow codomain, typing an effectful callback: `action: A => {Console} Unit`. See [[EffectfulType]].
    *
    * The brace may be **empty** — `{} A` is the row that adds nothing, i.e. "on my own ambient carrier" (effects-v5
    * step 1, docs/effects-v5-one-carrier.md §1). It is still a row, so it is still a carrier position: the `}` must be
    * followed by a type atom, which is what keeps an empty *transfer* brace (`: T {}`, nothing after it) and a `where`
    * guard out of this parser.
    *
    * A tail is read **with or without entries**. `{| G} A` is the pinned row at *n = 0*: the canonical stack of no
    * entries over `G`, which is the plain `G[A]`. It is spelled as a row not for the type — that needs no row — but
    * for the **tag**: a pinned position declares "this slot hosts a computation on this carrier", which is the one
    * thing about a user's own carrier that no declaration could state before (docs/effects.md W3). Same type,
    * different slot, exactly as §1 rule 2 already separates `{} A` from `G[A]`.
    */
  private lazy val effectfulTypeParser: Parser[Sourced[Token], Expression] = for {
    _          <- symbol("{")
    entries    <- component[UnresolvedAbilityConstraint[Sourced[Expression]]]
                    .atLeastOnceSeparatedBy(symbol(","))
                    .optional()
                    .map(_.getOrElse(Seq.empty))
    tail       <- rowTailParser
    _          <- symbol("}")
    resultType <- sourced(typeAtom)
  } yield EffectfulType(entries, resultType, tail)

  /** The optional `| base` tail of an effect row. Read whether or not entries precede it: with entries it names the
    * base of the canonical stack they build, and with none it is the stack at zero layers — the base itself, tagged as
    * a capture. See [[effectfulTypeParser]].
    */
  private def rowTailParser: Parser[Sourced[Token], Option[Sourced[Expression]]] =
    (symbol("|") *> sourced(typeRunParser)).optional()

  /** A named reference with an optional generic argument list `[…]` (always attached) and a value-argument list `(…)`
    * attached *only* when its `(` is adjacent to the preceding token (no intervening whitespace). The one call parser
    * for both value and type positions. Adjacency is what tells a value application `f(x)` / `if(c, T)` apart from an
    * infix operator followed by a parenthesized operand: in `X else (raise("…"))` the space after `else` keeps
    * `(raise("…"))` a separate atom, so the operator phase reads it as the operand of the infix `else` rather than as the
    * call `else(raise("…"))`. Inside the `(…)`/`[…]` the ordinary [[fullParser]] runs, so nested calls keep their usual
    * form. A non-infix `f (x)` is harmless — the operator phase re-applies the two operands into `f(x)`.
    */
  private lazy val adjacentCallParser: Parser[Sourced[Token], Expression] = for {
    prefix <- sourced(for {
                module   <- (moduleParser <* symbol("::")).atomic().optional()
                name     <- acceptIf(isIdentifierOrSymbol, "name")
                typeArgs <- presenceTrackingBracketedCommaSeparatedItems("[", sourced(fullParser), "]")
              } yield (module, name.map(_.content), typeArgs))
    args   <- valueArgsIfAdjacentTo(prefix.range.to)
  } yield {
    val (module, name, typeArgs) = prefix.value
    FunctionApplication(module, name, typeArgs, args.getOrElse(Seq.empty))
  }

  private def valueArgsIfAdjacentTo(prevEnd: Position): Parser[Sourced[Token], Option[Seq[Sourced[Expression]]]] =
    peekTokenStart.flatMap {
      case Some(from) if from === prevEnd => bracketedCommaSeparatedItems("(", sourced(fullParser), ")").optional()
      case _                              => Option.empty[Seq[Sourced[Expression]]].pure
    }

  /** Type atoms for the type positions (the single per-atom parser [[typeRunParser]] consumes a greedy *run* of these).
    * [[typeAtom]] (adjacency-sensitive, shared with value positions) extended with the effect-set sugar `{…} A`
    * ([[effectfulTypeParser]]): in a type run a `{` can only start an effect set (blocks are excluded from the type
    * surface), so the set is an ordinary atom — at the head of the run (`{Console} Unit`) or nested after an infix type
    * operator (`action: A => {Console} Unit`). The attempt is atomic: a `{…}` that is *not* an effect set — the
    * return-position transfer brace `: T {range(a) + …}`, an `implement` body after a `where` guard — backtracks cleanly
    * (its entries are not ability references, and no type atom ever follows a non-row brace: only `=`, `where`, a
    * definition keyword, or a closing delimiter can), leaving the `{` for the enclosing parser.
    */
  private lazy val typeRunAtom: Parser[Sourced[Token], Expression] =
    effectfulTypeParser.atomic() or
      parenthesizedExprParser.atomic() or
      adjacentCallParser or
      integerLiteralParser or
      stringLiteralParser

  /** The parser for **every type position** — function argument and return types, lambda-parameter annotations,
    * generic-parameter bounds and ability type-parameters, `type`-alias bodies, and `implement` patterns. It admits a
    * greedy **flat run of type atoms**, so an infix *type* operator reads naturally without parentheses: `f: A => B`
    * parses as a [[FlatExpression]] that `resolve`/`operator` lower to `=>(A, B)` (`Function[A, B]`), and an inline guard
    * return type `if(MIN > 0, A) else raise("…")` lowers to `else(if(MIN > 0, A), raise("…"))`, exactly as a body would. A
    * single atom is returned verbatim, so a plain `Int[0, 255]` / `IO[Unit]` is unchanged.
    *
    * The greedy run stops cleanly at a body's `=`, a closing/separating delimiter (`)`/`]`/`}`/`,`/`:`/`~`/`->`), or the
    * next definition, because every definition-introducing token (`def`/`type`/`implement`/…/`private`/`opaque`) is a
    * hard keyword and those delimiters are reserved symbols — none is a type-atom start (see [[Primitives.isUserOperator]]
    * and [[FunctionDefinition]]'s keyword note). So inside a `[…]`/`(…)` list it stops at the `,` separator and the
    * closing bracket, and a lambda-parameter annotation stops at the `->`. The effect-set sugar `{…} A` is an ordinary
    * type-run atom (see [[typeRunAtom]]), so it appears at the head of the run (`{Console} Unit`) or nested after an
    * infix type operator (`A => {Console} Unit`). Lambdas, `match`, and `{…}` blocks are deliberately excluded: they are
    * not part of the type-atom surface, which is what frees a type-position `{` to mean an effect set.
    */
  lazy val typeRunParser: Parser[Sourced[Token], Expression] =
    sourced(typeRunAtom).atLeastOnce().map {
      case Seq(single) => single.value
      case parts       => FlatExpression(parts)
    }

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] = fullParser
  }
}
