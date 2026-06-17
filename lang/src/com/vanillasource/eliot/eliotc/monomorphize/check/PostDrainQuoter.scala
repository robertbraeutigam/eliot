package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier => CoreQualifier}
import com.vanillasource.eliot.eliotc.module.fact.{UnifiedModuleValue, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.{Env, MetaStore, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter, SemExpressionEvaluator}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Post-drain conversion of a [[SemExpression]] tree into a [[MonomorphicExpression]] tree.
  *
  * Every [[SemValue]] slot is quoted via [[Quoter.quote]] against the final metastore. Unresolved metas, neutrals,
  * lambdas, native applications, and unapplied top-level definitions surface as explicit `Cannot resolve type.`
  * compiler errors (with source positions taken from the enclosing expression / name) — there is no silent
  * `GroundValue.Type` fallback.
  *
  * Ability references (value refs whose qualifier is [[CoreQualifier.Ability]]) are rewritten using the pre-computed
  * [[abilityResolutions]] map: refs resolved during the drain loop emit the chosen impl's FQN and type args; refs
  * absent from the map (constraint-covered calls where the concrete impl lives at the caller's level) emit their
  * original ability FQN so downstream dispatch stays abstract.
  *
  * '''Compile→runtime reification (the staging gate).''' A value-position sub-term whose value is fully determined by
  * erased `[]`-bound parameters (e.g. `name(A)` projecting a field of an erased `A: Person`, or an erased `BigInteger`
  * bound referenced directly) has no runtime slot for those parameters, so it cannot be emitted structurally. Instead
  * it is a compile-time constant and is '''materialised''' into a literal / constructor-call tree. The quoter walks the
  * tree top-down threading the names bound by enclosing runtime lambdas (`runtimeParams`) and an evaluation [[Env]]
  * seeded with the erased parameters (`monoEnv`). For each node:
  *   - references no erased parameter ⟹ recurse structurally; nothing is ever evaluated, so ordinary runtime code
  *     (including recursive defs and large constants) is preserved verbatim and cannot diverge.
  *   - references an erased parameter together with a runtime parameter or an inner lambda ⟹ the erased dependence
  *     resolves deeper; recurse structurally into the children.
  *   - references an erased parameter and nothing runtime ⟹ evaluate, force, and read back. A fully-ground result
  *     materialises to a literal/constructor tree; a result that does not reduce to a materialisable constant (a
  *     partial application's closure, a still-abstract head) recurses into the children if the node is an application,
  *     or — at a leaf that genuinely depends on an erased parameter with no constant value — raises a fail-safe
  *     compiler error (never a silent bad emit of a runtime reference to an erased parameter).
  */
class PostDrainQuoter(
    metaStore: MetaStore,
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    monoEnv: Env,
    lookupTopDef: ValueFQN => Option[SemValue]
) {

  private val erasedParams: Set[String]            = monoEnv.names.toSet
  private val semEvaluator: SemExpressionEvaluator = new SemExpressionEvaluator(lookupTopDef)

  /** Quote a [[SemValue]] to a [[GroundValue]]. Raises a sourced compiler error on failure. */
  def quoteSem(v: SemValue, at: Sourced[?]): CompilerIO[GroundValue] =
    Quoter.quote(0, v, metaStore) match {
      case Right(g)  => g.pure[CompilerIO]
      case Left(msg) => compilerAbort(at.as("Cannot resolve type."), Seq(msg))
    }

  /** Quote a sourced [[SemExpression]] tree to a sourced [[MonomorphicExpression]] tree, applying the staging gate. */
  def quoteSourced(expr: Sourced[SemExpression]): CompilerIO[Sourced[MonomorphicExpression]] =
    quoteSourcedIn(expr, Set.empty, monoEnv)

  private def quoteSourcedIn(
      expr: Sourced[SemExpression],
      runtimeParams: Set[String],
      evalEnv: Env
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    trySelectFold(expr, runtimeParams, evalEnv).getOrElse {
      val node       = expr.value.expression
      val refs       = collectParamRefs(node)
      val hasErased  = refs.exists(erasedParams.contains)
      val hasRuntime = refs.exists(runtimeParams.contains) || containsLambda(node)
      if (hasErased && !hasRuntime)
        tryMaterialise(expr, evalEnv).flatMap {
          case Some(mono) => mono.pure[CompilerIO]
          case None       =>
            node match {
              case _: SemExpression.FunctionApplication => structuralQuote(expr, runtimeParams, evalEnv)
              case _                                    =>
                compilerAbort(
                  expr.as("Value depends on a compile-time parameter but does not reduce to a constant.")
                )
            }
        }
      else structuralQuote(expr, runtimeParams, evalEnv)
    }

  /** Compile-time branch selection for the `Bool` eliminator `fold` (`WellKnownTypes.boolFoldFQN`) when its condition
    * is a compile-time constant.
    *
    * The width dispatch (`Int`'s `+`/`-`/`*` bodies) is a tree of `fold(fitsIn[...], leafA, leafB)` whose
    * '''condition''' is fully determined by erased type-stack bounds but whose '''branches''' carry runtime values
    * (`left`/`right`). The whole-node reification gate cannot fire (the branches are not erased-determined), yet the
    * fold must still collapse so the compile-time-only `fitsIn`/`fold` never reach the backend and only the one
    * selected leaf native survives.
    *
    * So: evaluate '''only the condition'''. If it forces to a ground `true`/`false`, emit the structurally-quoted
    * selected branch — keeping that branch's runtime content verbatim — and drop the other. A condition that stays
    * abstract (a genuine runtime `Bool`, e.g. user `fold(isEven, …)`) yields [[None]], leaving an ordinary `fold`
    * application for the normal read-back. This is exactly `fold`'s documented native semantics ("reduces when the
    * condition is a concrete `true`/`false`"), lifted to the expression read-back so runtime branch content is
    * preserved rather than evaluated to a constant.
    */
  private def trySelectFold(
      expr: Sourced[SemExpression],
      runtimeParams: Set[String],
      evalEnv: Env
  ): Option[CompilerIO[Sourced[MonomorphicExpression]]] =
    foldApplication(expr.value.expression).flatMap { case (cond, whenTrue, whenFalse) =>
      Evaluator.force(semEvaluator.eval(evalEnv, cond.value), metaStore) match {
        case SemValue.VConst(GroundValue.Direct(b: Boolean, _)) =>
          Some(quoteSourcedIn(if (b) whenTrue else whenFalse, runtimeParams, evalEnv))
        case _                                                  => None
      }
    }

  /** Match a three-argument application of the `Bool` eliminator `fold`, returning its `(condition, whenTrue,
    * whenFalse)` argument expressions. `fold`'s leading `[A]` type argument rides on the head value reference, so the
    * value spine is exactly the three arguments.
    */
  private def foldApplication(
      node: SemExpression.Expression
  ): Option[(Sourced[SemExpression], Sourced[SemExpression], Sourced[SemExpression])] =
    node match {
      case SemExpression.FunctionApplication(f2, whenFalse) =>
        f2.value.expression match {
          case SemExpression.FunctionApplication(f1, whenTrue) =>
            f1.value.expression match {
              case SemExpression.FunctionApplication(head, cond) =>
                head.value.expression match {
                  case SemExpression.ValueReference(vfqn, _) if vfqn.value === WellKnownTypes.boolFoldFQN =>
                    Some((cond, whenTrue, whenFalse))
                  case _                                                                                  => None
                }
              case _                                             => None
            }
          case _                                               => None
        }
      case _                                                => None
    }

  /** The normal structural read-back: quote this node's type slot and recurse into its children (unchanged from the
    * pre-reification behaviour, except that `runtimeParams` / `evalEnv` are threaded for the gate to use deeper).
    */
  private def structuralQuote(
      expr: Sourced[SemExpression],
      runtimeParams: Set[String],
      evalEnv: Env
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    for {
      exprType <- quoteSem(expr.value.expressionType, expr)
      inner    <- quoteExpression(expr.value.expression, expr, runtimeParams, evalEnv)
    } yield expr.as(MonomorphicExpression(exprType, inner))

  private def quoteExpression(
      expression: SemExpression.Expression,
      at: Sourced[?],
      runtimeParams: Set[String],
      evalEnv: Env
  ): CompilerIO[MonomorphicExpression.Expression] = expression match {
    case SemExpression.IntegerLiteral(v) =>
      (MonomorphicExpression.IntegerLiteral(v): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.StringLiteral(v) =>
      (MonomorphicExpression.StringLiteral(v): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.ParameterReference(name) =>
      (MonomorphicExpression.ParameterReference(name): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.FunctionApplication(target, argument) =>
      for {
        t <- quoteSourcedIn(target, runtimeParams, evalEnv)
        a <- quoteSourcedIn(argument, runtimeParams, evalEnv)
      } yield MonomorphicExpression.FunctionApplication(t, a)

    case SemExpression.FunctionLiteral(paramName, paramType, body) =>
      val innerEnv = evalEnv.bind(
        paramName.value,
        SemValue.VNeutral(SemValue.NeutralHead.VVar(evalEnv.level, paramName.value), SemValue.Spine.SNil)
      )
      for {
        pt <- quoteSem(paramType, paramName)
        b  <- quoteSourcedIn(body, runtimeParams + paramName.value, innerEnv)
      } yield MonomorphicExpression.FunctionLiteral(paramName, pt, b)

    case SemExpression.ValueReference(vfqn, typeArgs) if vfqn.value === WellKnownTypes.integerLiteralFQN =>
      // `integerLiteral[V]` is the platform-independent literal protocol (see `WellKnownTypes.integerLiteralFQN`): a
      // value-position literal `n` desugared so the checker could type it as `IntegerLiteralType[n]` (= `Int[n, n]`).
      // The reference carries the constant as its single erased type-argument `V`. This is a purely syntactic readback
      // rewrite — not evaluation: read `V` (already ground) and emit a plain `IntegerLiteral` node, which `structuralQuote`
      // stamps with the node's own already-computed `Int[n, n]` type. The backend's ordinary integer-literal path emits
      // it, so no `integerLiteral` reference survives into the monomorphic tree and no backend intrinsic is needed.
      typeArgs.traverse(a => quoteSem(a, vfqn)).flatMap {
        case Seq(GroundValue.Direct(v: BigInt, _)) =>
          (MonomorphicExpression.IntegerLiteral(vfqn.as(v)): MonomorphicExpression.Expression).pure[CompilerIO]
        case _                                     =>
          compilerAbort(vfqn.as("integerLiteral must have a single integer value argument."))
      }

    case SemExpression.ValueReference(vfqn, typeArgs) =>
      for {
        args <- typeArgs.traverse(a => quoteSem(a, vfqn))
      } yield resolveIfAbility(vfqn, args)
  }

  /** Evaluate a value-position sub-term that is determined by erased parameters, then read it back and materialise it
    * into a literal / constructor-call tree. Returns [[None]] when the result does not reduce to a materialisable
    * constant (a closure, a still-abstract head, a type) — the caller decides whether to recurse or fail.
    */
  private def tryMaterialise(
      expr: Sourced[SemExpression],
      evalEnv: Env
  ): CompilerIO[Option[Sourced[MonomorphicExpression]]] = {
    val forced = Evaluator.force(semEvaluator.eval(evalEnv, expr.value), metaStore)
    Quoter.quote(0, forced, metaStore) match {
      case Left(_)       => Option.empty[Sourced[MonomorphicExpression]].pure[CompilerIO]
      case Right(ground) => materialise(ground, expr).map(_.map(expr.as))
    }
  }

  /** Expand a fully-ground compile-time value into a [[MonomorphicExpression]] tree.
    *
    *   - `Direct(BigInt)` / `Direct(String)` ⟹ the matching literal node.
    *   - `Direct(Boolean)` ⟹ a reference to the platform `Bool` constructor (`true` / `false`).
    *   - `Structure` headed by a concrete value constructor ⟹ a (curried) constructor application; the leading
    *     type-arguments stay type arguments and the trailing field values become applications (Stage 2).
    *
    * Returns [[None]] for anything that is not a runtime constant: a `Type`, a non-primitive `Direct` (e.g. the unit
    * placeholder), or a `Structure` whose head is not a concrete value constructor (an abstract / type-constructor head
    * — "defined on both sides" is not met). The caller surfaces that as a recurse-or-fail decision.
    */
  private def materialise(ground: GroundValue, at: Sourced[?]): CompilerIO[Option[MonomorphicExpression]] =
    ground match {
      case GroundValue.Direct(v: BigInt, vt)  =>
        Some(MonomorphicExpression(vt, MonomorphicExpression.IntegerLiteral(at.as(v)))).pure[CompilerIO]
      case GroundValue.Direct(v: String, vt)  =>
        Some(MonomorphicExpression(vt, MonomorphicExpression.StringLiteral(at.as(v)))).pure[CompilerIO]
      case GroundValue.Direct(v: Boolean, vt) =>
        val ctor = if (v) WellKnownTypes.boolTrueFQN else WellKnownTypes.boolFalseFQN
        Some(MonomorphicExpression(vt, MonomorphicExpression.MonomorphicValueReference(at.as(ctor), Seq.empty)))
          .pure[CompilerIO]
      case GroundValue.Direct(_, _)           =>
        Option.empty[MonomorphicExpression].pure[CompilerIO]
      case s: GroundValue.Structure           =>
        materialiseStructure(s, at)
      case GroundValue.Type                   =>
        Option.empty[MonomorphicExpression].pure[CompilerIO]
    }

  /** Materialise a data value as a constructor application (Stage 2). The constructor's field arity comes from its
    * [[RoleHint.ValueConstructor]] — the one sanctioned read of constructor-shape metadata, as `match` reconstruction
    * also uses (only `fieldCount`, never `typeParamCount`). The quoted structure's leading args are the constructor's
    * type arguments (kept as `MonomorphicValueReference` type args) and the trailing `fieldCount` args are the field
    * values (materialised recursively and applied). A head that is not a concrete value constructor yields [[None]].
    */
  private def materialiseStructure(
      s: GroundValue.Structure,
      at: Sourced[?]
  ): CompilerIO[Option[MonomorphicExpression]] =
    constructorRole(s.typeName).flatMap {
      case None                                                  =>
        Option.empty[MonomorphicExpression].pure[CompilerIO]
      case Some(RoleHint.ValueConstructor(dataType, fieldCount)) =>
        val splitIndex = s.args.size - fieldCount
        if (splitIndex < 0) Option.empty[MonomorphicExpression].pure[CompilerIO]
        else {
          val (typeArgs, valueArgs)       = s.args.splitAt(splitIndex)
          // The value's type is the data type constructor (same module as the constructor) applied to the type args.
          val valueType                   =
            GroundValue.Structure(ValueFQN(s.typeName.moduleName, dataType), typeArgs, GroundValue.Type)
          val base: MonomorphicExpression =
            MonomorphicExpression(
              valueType,
              MonomorphicExpression.MonomorphicValueReference(at.as(s.typeName), typeArgs)
            )
          valueArgs.foldLeftM(Option(base)) {
            case (None, _)              => Option.empty[MonomorphicExpression].pure[CompilerIO]
            case (Some(acc), argGround) =>
              materialise(argGround, at).map(_.map { argExpr =>
                // Intermediate application types are discarded by the uncurry pass (only the outermost, = `valueType`,
                // and the leaf argument types survive), so reusing `valueType` here is safe.
                MonomorphicExpression(
                  valueType,
                  MonomorphicExpression.FunctionApplication(at.as(acc), at.as(argExpr))
                )
              })
          }
        }
      case Some(_)                                               =>
        Option.empty[MonomorphicExpression].pure[CompilerIO]
    }

  /** The [[RoleHint]] of a value FQN, or [[None]] if no module value is known for it. */
  private def constructorRole(fqn: ValueFQN): CompilerIO[Option[RoleHint]] =
    getFact(UnifiedModuleValue.Key(fqn)).map(_.map(_.namedValue.roleHint))

  /** All parameter names referenced anywhere in this expression node's subtree. Type-argument [[SemValue]]s of a value
    * reference are not walked — they are erased and never produce a runtime reference, so they do not gate
    * materialisation of the surrounding value.
    */
  private def collectParamRefs(expression: SemExpression.Expression): Set[String] = expression match {
    case SemExpression.ParameterReference(name)         => Set(name.value)
    case SemExpression.FunctionApplication(target, arg) =>
      collectParamRefs(target.value.expression) ++ collectParamRefs(arg.value.expression)
    case SemExpression.FunctionLiteral(_, _, body)      => collectParamRefs(body.value.expression)
    case _                                              => Set.empty
  }

  /** Whether this expression node's subtree contains a function literal (an inner runtime lambda). */
  private def containsLambda(expression: SemExpression.Expression): Boolean = expression match {
    case SemExpression.FunctionLiteral(_, _, _)         => true
    case SemExpression.FunctionApplication(target, arg) =>
      containsLambda(target.value.expression) || containsLambda(arg.value.expression)
    case _                                              => false
  }

  private def resolveIfAbility(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[GroundValue]
  ): MonomorphicExpression.Expression =
    vfqn.value.name.qualifier match {
      case _: CoreQualifier.Ability =>
        abilityResolutions.get(vfqn) match {
          case Some((implFqn, implTypeArgs)) =>
            MonomorphicExpression.MonomorphicValueReference(vfqn.as(implFqn), implTypeArgs)
          case None                          =>
            MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
        }
      case _                        =>
        MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
    }
}
