package com.vanillasource.eliot.eliotc.row

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.collection.mutable

/** **The write** — effects v6, `docs/effects.md` §9.4 step 3. Rewrites one definition so that every reference carries
  * the implementation each of the callee's **phantom binders** stands for.
  *
  * It replaces `RowElaborator`. There is no carrier to solve for, so there is no `flatMap` to hoist, no `pure` or
  * `runId` to insert and no discharge stack to derive: an operation call is an ordinary call, and the only thing
  * missing from it is which implementation it runs on. Three jobs, one walk:
  *
  *   1. **write the bindings.** At each reference the callee's phantom binders are read off its declaration, and each
  *      is given a value by the resolution order below, as a leading positional prefix. A binder is never left to a
  *      metavariable.
  *   2. **thunk and apply.** A row-typed slot lowered to `Unit => A` (§9.4 step 2), so an actual delivered there is
  *      wrapped in a lambda, and a reference to one of *this* definition's row-typed parameters is applied to `unit`.
  *      Doing both unconditionally is what makes a pass-through (`runAbort(computation)`,
  *      `foldOption(fallback, …)`, `val restFailures = rest`) come out right with no inspection of the argument's
  *      shape or type: wrap and apply are inverse, so a pass-through is an η-expansion and nothing more.
  *   3. **erase `with`.** The node exists to put a binding in scope for its subject; once the subject's references
  *      carry it, it is dropped — from a body, and from a slot's type in the signature.
  *
  * **Where a binding comes from — the resolution order** (§9.4), walking outward lexically:
  *
  *   1. the nearest enclosing `with` for that ability;
  *   2. this definition's own phantom binder for it — a *received* binding, filled by its caller. There is no graph to
  *      sum: forwarding is the enclosing signature, read once;
  *   3. for an actual at a row-typed slot, the entries that slot **supplies** — bound by the slot's own `with`, or by
  *      `Default` with none. An entry the callee's own declared row already has is *not* supplied, and the walk
  *      continues outward (Part I's supplied-versus-rides rule, §2.2, unchanged);
  *   4. `Default` — "search at the ground arguments", today's two-site resolution — for an ordinary **ability**. For
  *      an **effect** there is no default: an uncovered one is the "performs but does not declare" error, reported
  *      here at the reference, because this walk is exactly the scope check's walk.
  *
  * **Effect-ness is read from one place only**: the callee's declared row. An ability appearing in
  * `effectRow.returnEffects` is an effect at this reference — which for an `effect`'s member is what membership
  * recorded ([[com.vanillasource.eliot.eliotc.ast.fact.AbilityMembers]]) and for an ordinary definition is what its
  * `{ … }` says. A `~` constraint's ability is in no row, so it defaults. Nothing keys on a name or a shape.
  *
  * The walk crosses a lambda boundary iff the lambda's slot has a row (D5, §9.4): a lambda at a rowless arrow may bind
  * and discharge locally but never reaches the enclosing declarations. That falls out of the walk rather than being a
  * rule of its own — a rowless slot is not entered with a slot scope, and a lambda's own parameter shadows a thunk of
  * the same name.
  */
object BindingWriter {

  /** @param value
    *   The definition with its body written and its signature stripped of slot `with`s.
    * @param violations
    *   What the walk could not answer, each at its own position. A violation aborts the definition: an unwritten
    *   binding would silently run on the platform's default.
    */
  case class Written(value: OperatorResolvedValue, violations: Seq[Violation])

  case class Violation(message: Sourced[String], help: Seq[String] = Seq.empty)

  /** A binding in scope: an implementation for one ability, and the term naming it. */
  private case class Binding(ability: AbilityFQN, term: OperatorResolvedExpression)

  /** The lexical environment at one point. `bindings` is innermost-first, so a nearer `with` shadows an outer one for
    * the same ability; `thunks` are the row-typed parameters in scope, whose references apply.
    */
  private case class Scope(bindings: Seq[Binding], thunks: Set[String]) {
    def bind(binding: Binding): Scope = copy(bindings = binding +: bindings)
    def shadow(name: String): Scope   = copy(thunks = thunks - name)

    def lookup(ability: AbilityFQN): Option[OperatorResolvedExpression] =
      bindings.find(_.ability == ability).map(_.term)
  }

  /** @param atBoundary
    *   True for a platform **run boundary** ([[RunBoundaryFunctions]]) — the synthesized entry point. It is where every
    *   effect's chain ends (§9.5), so an uncovered effect is bound to the two-site `Default` there instead of being
    *   reported undeclared. Everywhere else an uncovered effect is the error, which is the whole of the scope check.
    */
  def write(orv: OperatorResolvedValue, universe: RowChecker.Universe, atBoundary: Boolean = false): Written = {
    val writer = new Writer(universe, atBoundary)
    val scope  = Scope(receivedBindings(orv, universe), thunkParameters(orv))
    val view   = SignatureView.of(orv.signature)
    val body   = orv.runtime.map(writer.walkDefinition(_, scope, view.binders.size + view.parameters.size))
    Written(
      orv.copy(runtime = body.orElse(orv.runtime), signature = strippedSignature(orv)),
      writer.violations.toSeq
    )
  }

  /** The bindings a definition receives from its caller — resolution-order step 2 — one per phantom binder of its own
    * signature, naming that binder.
    */
  private def receivedBindings(orv: OperatorResolvedValue, universe: RowChecker.Universe): Seq[Binding] = {
    val binders = SignatureView.of(orv.signature).binders
    phantoms(orv).map { case (index, ability) =>
      Binding(ability, ParameterReference(binders(index).name))
    }
  }

  /** The names of this definition's row-typed value parameters: a reference to one runs it. */
  private def thunkParameters(orv: OperatorResolvedValue): Set[String] = {
    val view            = SignatureView.of(orv.signature)
    val (names, _)      = RowChecker.peelBinders(orv.runtime.map(_.value).getOrElse(view.returnType.value))
    val valueParamNames = names.takeRight(view.parameters.size)
    orv.effectRow.parameterEffects.flatMap(pe => valueParamNames.lift(pe.parameterIndex)).toSet
  }

  /** A definition's **phantom binders**, as `(index, ability)` pairs in index order, read off its declaration alone.
    *
    * Two shapes, each keyed on something the compiler owns:
    *
    *   - an **ability member** (`Qualifier.Ability`) carries its block's binding slot at index 0
    *     ([[com.vanillasource.eliot.eliotc.ast.fact.AbilityMembers]]), for the ability whose module it lives in;
    *   - any other definition's minted binders are its leading binders that occur in **no parameter and no return
    *     type** — which is what "in no type" means — *and* are the **first** type argument of one of its own ability
    *     constraints. Requiring the second half is what keeps a merely unused type parameter from being taken for a
    *     binding.
    *
    * The result is always a contiguous prefix from index 0, because a type-argument list applies positionally and the
    * write is a prefix write; [[nonPrefixPhantom]] reports the one declaration shape that would break that.
    */
  private def phantoms(orv: OperatorResolvedValue): Seq[(Int, AbilityFQN)] =
    orv.name.value.qualifier match {
      case ResolveQualifier.Ability(name) => Seq(0 -> AbilityFQN(orv.vfqn.moduleName, name))
      case _                              => prefixOf(mintedPhantoms(orv))
    }

  /** The leading run whose indices are 0, 1, 2, … — all of them for anything the desugar produced, since it mints at
    * the front. Anything past a gap is unreachable by a positional write and is reported by [[nonPrefixPhantom]].
    */
  private def prefixOf(minted: Seq[(Int, AbilityFQN)]): Seq[(Int, AbilityFQN)] =
    minted.zipWithIndex.takeWhile { case ((index, _), position) => index === position }.map(_._1)

  private def mintedPhantoms(orv: OperatorResolvedValue): Seq[(Int, AbilityFQN)] = {
    val view      = SignatureView.of(orv.signature)
    val mentioned = (view.parameters :+ view.returnType).flatMap(t => referencedParameters(t.value)).toSet
    view.binders.zipWithIndex.flatMap { case (binder, index) =>
      Option
        .when(!mentioned.contains(binder.name.value))(binder.name.value)
        .flatMap(name => constraintStartingWith(orv, name))
        .map(index -> _)
    }
  }

  /** An ability member whose *own* row mints a binder past the ability-level prefix: the write would have to spell the
    * ability's pattern arguments to reach it, and no declaration determines those. Reported rather than mis-written.
    */
  private def nonPrefixPhantom(orv: OperatorResolvedValue): Option[Sourced[String]] =
    orv.name.value.qualifier match {
      case ResolveQualifier.Ability(_) if mintedPhantoms(orv).nonEmpty =>
        Some(
          orv.name.as(
            s"The member '${orv.vfqn.name.name}' declares effects of its own beyond the ability it belongs to, " +
              "which is not supported: move it out of the block and give it its own row."
          )
        )
      case ResolveQualifier.Ability(_)                                 => None
      case _                                                           =>
        val minted = mintedPhantoms(orv)
        Option.when(prefixOf(minted).size =!= minted.size)(
          orv.name.as(
            s"Cannot write the implementations of '${orv.vfqn.name.name}': one of its binders is behind a type " +
              "parameter no declaration determines."
          )
        )
    }

  /** The ability of the definition's own constraint whose **first** type argument is exactly this binder — where the
    * desugar writes a phantom binder, and where an ability's marker declares its binding slot.
    */
  private def constraintStartingWith(orv: OperatorResolvedValue, binderName: String): Option[AbilityFQN] =
    orv.paramConstraints.values.flatten
      .find(_.typeArgs.headOption.exists {
        case ParameterReference(name) => name.value === binderName
        case _                        => false
      })
      .map(_.abilityFQN)

  private def referencedParameters(expr: OperatorResolvedExpression): Seq[String] = expr match {
    case ParameterReference(name)             => Seq(name.value)
    case FunctionApplication(target, arg)     =>
      referencedParameters(target.value) ++ referencedParameters(arg.value)
    case ValueReference(_, typeArgs)          => typeArgs.flatMap(ta => referencedParameters(ta.value))
    case FunctionLiteral(_, paramType, body)  =>
      paramType.toSeq.flatMap(pt => referencedParameters(pt.value)) ++ referencedParameters(body.value)
    case WithBinding(subject, _)              => referencedParameters(subject.value)
    case _: IntegerLiteral | _: StringLiteral => Seq.empty
  }

  /** This definition's signature with every slot `with` removed — it is a declaration about the slot, not part of its
    * type, and nothing past this phase knows the node.
    */
  private def strippedSignature(orv: OperatorResolvedValue): Sourced[OperatorResolvedExpression] =
    orv.signature.map(stripWith)

  private def stripWith(expr: OperatorResolvedExpression): OperatorResolvedExpression = expr match {
    case WithBinding(subject, _) => stripWith(subject.value)
    case other                   => OperatorResolvedExpression.mapChildrenM[cats.Id](stripWith)(other)
  }

  /** The implementations a slot's type binds, outermost `with` last, as written. */
  private def slotBindings(tpe: OperatorResolvedExpression): Seq[Sourced[ValueFQN]] = tpe match {
    case WithBinding(subject, implementation) => slotBindings(subject.value) :+ implementation
    case _                                    => Seq.empty
  }

  /** The ability an implementation marker implements, read off its **resolved qualifier** — never off its name. */
  private def abilityOf(marker: ValueFQN, universe: RowChecker.Universe): Option[AbilityFQN] =
    universe.lookup(marker).flatMap(_.name.value.qualifier match {
      case ResolveQualifier.AbilityImplementation(ability, _, _) => Some(ability)
      case _                                                     => None
    })

  private class Writer(universe: RowChecker.Universe, atBoundary: Boolean) {
    val violations: mutable.Buffer[Violation] = mutable.Buffer.empty

    /** The definition's own leading binders — its generic binders and then its value parameters — are peeled without
      * shadowing, because they are what *put* the row-typed parameters in scope. Only a lambda inside the body proper
      * can shadow one.
      */
    def walkDefinition(
        expr: Sourced[OperatorResolvedExpression],
        scope: Scope,
        remaining: Int
    ): Sourced[OperatorResolvedExpression] =
      expr.value match {
        case FunctionLiteral(paramName, paramType, body) if remaining > 0 =>
          expr.as(FunctionLiteral(paramName, paramType, walkDefinition(body, scope, remaining - 1)))
        case _                                                           => walk(expr, scope)
      }

    def walk(expr: Sourced[OperatorResolvedExpression], scope: Scope): Sourced[OperatorResolvedExpression] =
      expr.value match {
        // A `with` puts its implementation in scope for its subject, innermost first, and disappears.
        case WithBinding(subject, implementation) =>
          abilityOf(implementation.value, universe) match {
            case Some(ability) =>
              walk(subject, scope.bind(Binding(ability, ValueReference(implementation))))
            case None          =>
              violations += Violation(implementation.as("This name is not an implementation."))
              walk(subject, scope)
          }

        // A reference to one of this definition's row-typed parameters is a thunk: running it is applying it.
        case ParameterReference(name) if scope.thunks.contains(name.value) =>
          expr.as(FunctionApplication(expr, unitValue(expr)))

        case FunctionLiteral(paramName, paramType, body) =>
          expr.as(FunctionLiteral(paramName, paramType, walk(body, scope.shadow(paramName.value))))

        case _: IntegerLiteral | _: StringLiteral | _: ParameterReference => expr

        case _ =>
          val (head, args) = spine(expr.value)
          head match {
            case ValueReference(callee, existing) =>
              val written  = writeBindings(expr.as(head), callee.value, existing, scope)
              val adjusted = args.zipWithIndex.map { case (arg, index) => walkArgument(arg, callee.value, index, scope) }
              expr.as(applyChain(written, adjusted))
            case _                                =>
              expr.as(applyChain(walk(expr.as(head), scope), args.map(walk(_, scope))))
          }
      }

    /** One actual, at the callee's parameter `index`. A row-typed slot is a thunk, so the actual is wrapped — and the
      * entries that slot *supplies* are bound inside it, which is resolution-order step 3.
      */
    private def walkArgument(
        arg: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        index: Int,
        scope: Scope
    ): Sourced[OperatorResolvedExpression] =
      universe.lookup(callee).flatMap(orv => rowSlot(orv, index).map(orv -> _)) match {
        case None                    => walk(arg, scope)
        case Some((calleeOrv, slot)) =>
          val name    = thunkParameter
          val inside  = suppliedScope(calleeOrv, index, slot, scope, arg).shadow(name)
          arg.as(
            FunctionLiteral(
              arg.as(name),
              Some(arg.as(ValueReference(arg.as(WellKnownTypes.unitTypeFQN)))),
              walk(arg, inside)
            )
          )
      }

    /** The scope inside a row-typed slot: every entry the slot **supplies** — one the callee's own declared row does
      * not already have — bound to the slot's `with` for that ability, or to `Default`.
      */
    private def suppliedScope(
        calleeOrv: OperatorResolvedValue,
        index: Int,
        slot: Seq[AbilityFQN],
        scope: Scope,
        anchor: Sourced[?]
    ): Scope = {
      val rides    = calleeOrv.effectRow.returnEffects.map(_.abilityFQN).toSet
      val declared = slotBindings(SignatureView.of(calleeOrv.signature).parameters(index).value)
        .flatMap(marker => abilityOf(marker.value, universe).map(_ -> marker))
        .toMap
      slot.filterNot(rides).foldLeft(scope) { (acc, ability) =>
        acc.bind(Binding(ability, declared.get(ability).fold(defaultBinding(anchor))(m => ValueReference(m))))
      }
    }

    /** The abilities a callee's parameter `index` declares, when that parameter is a row position at all. */
    private def rowSlot(orv: OperatorResolvedValue, index: Int): Option[Seq[AbilityFQN]] =
      orv.effectRow.parameterEffects.find(_.parameterIndex === index).map(_.effects.map(_.abilityFQN))

    /** Write the callee's phantom binders as a leading positional prefix. */
    private def writeBindings(
        reference: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        existing: Seq[Sourced[OperatorResolvedExpression]],
        scope: Scope
    ): Sourced[OperatorResolvedExpression] =
      universe.lookup(callee) match {
        case None      => reference
        case Some(orv) =>
          nonPrefixPhantom(orv).foreach(message => violations += Violation(message))
          phantoms(orv) match {
            case Seq()      => reference
            case theirs     =>
              val effects = orv.effectRow.returnEffects.map(_.abilityFQN).toSet
              val prefix  = theirs.map { case (_, ability) =>
                reference.as(bindingFor(ability, effects.contains(ability), reference, scope))
              }
              reference.as(ValueReference(nameOf(reference), prefix ++ existing))
          }
      }

    /** One binder's value. An **ability** with nothing in scope defaults to the two-site search; an **effect** with
      * nothing in scope is the "performs but does not declare" error, at this reference.
      */
    private def bindingFor(
        ability: AbilityFQN,
        isEffect: Boolean,
        at: Sourced[?],
        scope: Scope
    ): OperatorResolvedExpression =
      scope.lookup(ability) match {
        case Some(term)                     => term
        case None if isEffect && !atBoundary =>
          violations += Violation(
            at.as(
              s"This value performs the effect '${ability.abilityName}' but does not declare it; " +
                "add it to its { ... } effect set."
            ),
            Seq(s"Or bind an implementation for it here with `with`.")
          )
          defaultBinding(at)
        case None            => defaultBinding(at)
      }

    private def nameOf(reference: Sourced[OperatorResolvedExpression]): Sourced[ValueFQN] =
      reference.value match {
        case ValueReference(name, _) => name
        case other                   =>
          throw IllegalStateException(s"An application head is not a value reference: ${other.render}")
      }
  }

  /** The `Default` sentinel: "search at the ground arguments", today's two-site resolution. */
  private def defaultBinding(at: Sourced[?]): OperatorResolvedExpression =
    ValueReference(at.as(WellKnownTypes.defaultImplementationFQN))

  private def unitValue(at: Sourced[?]): Sourced[OperatorResolvedExpression] =
    at.as(ValueReference(at.as(WellKnownTypes.unitValueFQN)))

  /** The name a thunk's ignored parameter binds. It shadows nothing a user can write, and every thunk uses the same
    * one because a thunk's body never mentions it.
    */
  private val thunkParameter = "$unit"
}
