package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{
  acceptIfAll,
  between,
  optional,
  or,
  recoveringAnyTimes,
  recoveringAtLeastOnce
}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object ImplementBlock {
  val implementBlock: ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] =
    new ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], Seq[FunctionDefinition])] =
        for {
          _                   <- keyword("implement")
          genericParameters   <- component[Seq[GenericParameter]]
          name                <- acceptIfAll(isIdentifier, isUpperCase)("ability name")
          pattern             <- bracketedCommaSeparatedItems("[", sourced(Expression.typeRunParser), "]")
          // Optional `where <guard>` clause (ability-guards Stage 1). `where` is a soft keyword (a lowercase
          // identifier in a unique position — no tokenizer change). The guard is parsed with `typeRunParser` — the
          // same parser as a return type — so an infix guard like `E1 != E2` reads without parentheses and the run
          // stops cleanly at the body's `{` (`{` is not a type-atom start), giving the parse boundary of §2.1 for
          // free. It rides the marker's return-type slot below.
          guard               <- (identifierWith("where") *> sourced(Expression.typeRunParser)).optional()
          (errors, functions) <-
            (component[FunctionDefinition] or TypeAliasDefinition.typeAliasDefinition.parser)
              .recoveringAtLeastOnce(t => isKeyword(t) && (hasContent("def")(t) || hasContent("type")(t)))
              .between(symbol("{"), symbol("}"))
              .optional()
              .map(_.getOrElse(Seq.empty, Seq.empty))
          // The implementation's identity: a canonical, position-independent string of *what* it implements — the
          // surface form of its type-argument pattern plus its `where` guard. Two `implement` blocks are the same
          // instance iff this key matches (independent of file/position/order), which is what lets an instance be split
          // across layers and merged, while keeping distinct patterns (`Eq[Type]` vs `Eq[String]`) and a guarded
          // instance (whose `where` clause, e.g. the `Coerce[Int, Int]` widening guard, is part of its identity) apart
          // from an otherwise same-pattern sibling. `Show[Expression]` drops
          // source positions, so the key is stable across layers that spell the pattern identically (the same
          // character-exact discipline the layer merge already requires).
          patternKey           = pattern.map(_.value.show).mkString(", ") +
            guard.map(g => s" where ${g.value.show}").getOrElse("")
        } yield (
          errors,
          functions.map(f =>
            // Transform the function into an "implement" function. Change name into implement qualifier,
            // and also prepend the common generic parameters.
            // Note: the implement qualifier has to include the instantiation type parameters
            // Visibility is always public for implementation functions.
            FunctionDefinition(
              f.name.map(n => QualifiedName(n.name, Qualifier.AbilityImplementation(name.value.content, patternKey))),
              genericParameters ++ f.genericParameters,
              f.args,
              f.typeDefinition,
              f.body,
              visibility = Visibility.Public,
              opaque = f.opaque
            )
          ) :+
            // We add the implementation to the default method as a marker, that this type implements the marker.
            // The marker takes one argument per pattern element so its signature fully encodes the pattern,
            // which is what drives implementation selection during dispatch.
            //
            // The marker is a pure signature vessel: its return-type slot carries the guard (the `where <expr>`, or
            // the literal `true` when absent) and it is body-less. This co-opts the return slot rather than threading
            // a new `guard` field through the whole front-end; matching is unaffected because `AbilityMatcher` drops
            // the return before unifying the pattern arguments. The former identity body (`arg0`) is retired
            // uniformly so it never fights the guard at concrete discharge (ability-guards §2.3).
            FunctionDefinition(
              name.as(
                QualifiedName(name.value.content, Qualifier.AbilityImplementation(name.value.content, patternKey))
              ),
              genericParameters,
              pattern.zipWithIndex.map { case (p, i) => ArgumentDefinition(name.as(s"arg$i"), p) },
              guard.getOrElse(Expression.trueReference(name)),
              None
            )
        )
    }
}
