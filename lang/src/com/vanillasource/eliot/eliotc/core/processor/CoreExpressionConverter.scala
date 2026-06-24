package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  GenericParameter,
  ArgumentDefinition as SourceArgument,
  Expression as SourceExpression,
  LambdaParameterDefinition as SourceLambdaParameter,
  PrecedenceDeclaration as AstPrecedenceDeclaration,
  Pattern as SourcePattern
}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Converts source AST expressions, types, and patterns into core AST expressions. All methods are pure, stateless
  * transformations.
  */
object CoreExpressionConverter {

  /** Converts a source expression to a core expression. In type context, bare uppercase names get Qualifier.Type; in
    * body context, they get Qualifier.Default (unless they have [] args, which always implies type-level). Value-level
    * () args are always converted in body context, [] args always in type context.
    */
  def convertExpression(expr: Sourced[SourceExpression], typeContext: Boolean): Sourced[Expression] =
    expr.value match {
      case SourceExpression.FunctionApplication(moduleName, fnName, genericArgs, args) =>
        val isUpper = fnName.value.charAt(0).isUpper
        // An upper-case name (not applied to value `()` args) resolves in the Type namespace when it is in type
        // context, OR when `[...]` brackets are present at all — including an explicit empty `[]`. The empty `[]` is
        // the type-level analogue of value-level `()`: it forces a bare type constructor (e.g. `JvmByte[]`) into the
        // Type namespace in value position, where a plain `JvmByte` would default to a (possibly non-existent) value
        // constructor.
        if (isUpper && args.isEmpty && (typeContext || genericArgs.isDefined)) {
          val base = expr.as(NamedValueReference(fnName.map(n => QualifiedName(n, Qualifier.Type)), moduleName))
          curryApplicationWith(base, genericArgs.getOrElse(Seq.empty), convertExpression(_, typeContext = true))
        } else {
          curryApplicationWith(
            expr.as(
              NamedValueReference(
                fnName.map(n => QualifiedName(n, Qualifier.Default)),
                moduleName,
                genericArgs.getOrElse(Seq.empty).map(convertExpression(_, typeContext = true))
              )
            ),
            args,
            convertExpression(_, typeContext = false)
          )
        }
      case SourceExpression.FunctionLiteral(params, body)                              =>
        curryLambda(params, convertExpression(body, typeContext))
      case SourceExpression.IntegerLiteral(lit)                                        =>
        if (typeContext) {
          // Type/bound position (inside `[...]`): the literal is a bare `BigInteger` bound.
          expr.as(IntegerLiteral(lit))
        } else {
          // Value position: desugar `n` into `integerLiteral[n] : Int[n, n]`. The inner `[n]` lands in
          // `typeArgs` (type context), so it stays a bare `BigInteger`.
          expr.as(
            NamedValueReference(
              lit.as(QualifiedName("integerLiteral", Qualifier.Default)),
              None,
              Seq(expr.as(IntegerLiteral(lit)))
            )
          )
        }
      case SourceExpression.StringLiteral(lit)                                         =>
        expr.as(StringLiteral(lit))
      case SourceExpression.FlatExpression(Seq(single))                                =>
        convertExpression(single, typeContext)
      case SourceExpression.FlatExpression(parts)                                      =>
        expr.as(FlatExpression(parts.map(p => p.as(TypeStack.of(convertExpression(p, typeContext).value)))))
      case SourceExpression.MatchExpression(scrutinee, cases)                          =>
        expr.as(
          MatchExpression(
            scrutinee.as(TypeStack.of(convertExpression(scrutinee, typeContext).value)),
            cases.map(c =>
              MatchCase(
                c.pattern.map(toPattern),
                c.body.as(TypeStack.of(convertExpression(c.body, typeContext).value))
              )
            )
          )
        )
      case SourceExpression.BlockExpression(lines)                                     =>
        expr.as(BlockExpression(lines.map { line =>
          BlockLine(
            line.binder.map(_.name),
            line.binder.flatMap(_.typeExpression).map(t => TypeStack.of(convertExpression(t, typeContext = true).value)),
            convertExpression(line.expression, typeContext)
          )
        }))
      case _: SourceExpression.EffectfulType                                           =>
        // EffectSugarDesugarer rewrites every `{…} A` to `F[A]` across the whole function before conversion, so an
        // EffectfulType reaching here means it was written somewhere the desugarer does not reach (only signature and
        // body positions are handled). Fail loudly rather than mis-convert it.
        throw IllegalStateException(
          s"Effect braces `{…}` are only supported in function signature positions: ${expr.value.show}"
        )
    }

  /** Builds a curried function type. So f[A, B](d: D, e: E): F type becomes: A -> B -> Function^Type(D,
    * Function^Type(E, F)). Note: f[A, M[_]]... becomes: A -> M -> ..., where M has a type expression on it: X -> Y \->
    * Function^Type(X, Y)
    */
  def curriedFunctionType(
      args: Seq[SourceArgument],
      returnType: Sourced[SourceExpression],
      genericParams: Seq[GenericParameter]
  ): Sourced[Expression] = {
    val withArgs = args.foldRight[Sourced[Expression]](convertExpression(returnType, typeContext = true)) {
      (arg, acc) =>
        val argType     = convertExpression(arg.typeExpression, typeContext = true)
        val functionRef =
          arg.name.as(NamedValueReference(arg.name.as(QualifiedName("Function", Qualifier.Type))))
        val withArgType = arg.name.as(
          FunctionApplication(
            functionRef,
            argType
          )
        )
        arg.name.as(
          FunctionApplication(
            withArgType,
            acc
          )
        )
    }
    genericParams.foldRight[Sourced[Expression]](withArgs) { (param, acc) =>
      val kindType = convertExpression(param.typeRestriction, typeContext = true)
      param.name.as(FunctionLiteral(param.name, Some(TypeStack.of(kindType.value)), acc.map(TypeStack.of)))
    }
  }

  /** Converts the body into core expression and embeds it as a lambda with the "function" parameters.
    *
    * The parameters are left *unannotated*: a value's type stack (signature) is the single source of truth for its
    * parameter types, and the body is always *checked* against that signature, so each parameter takes its type from
    * the signature's corresponding `Function` domain (see the `check(FunctionLiteral, VPi)` case in the monomorphize
    * `Checker`). Re-stating the type on the body lambda would only duplicate it — and would force later passes that
    * refine the signature (e.g. `auto`-parameter saturation) to keep a redundant body copy in sync.
    */
  def buildCurriedBody(
      args: Seq[SourceArgument],
      value: Sourced[SourceExpression],
      typeContext: Boolean = false
  ): Sourced[Expression] =
    args.foldRight(convertExpression(value, typeContext)) { (arg, acc) =>
      arg.name.as(FunctionLiteral(arg.name, None, acc.map(TypeStack.of)))
    }

  def toPattern(pattern: SourcePattern): Pattern =
    pattern match {
      case SourcePattern.ConstructorPattern(moduleName, constructorName, subPatterns, isTypePattern) =>
        val qualifier = if (isTypePattern) Qualifier.Type else Qualifier.Default
        Pattern.ConstructorPattern(
          moduleName,
          constructorName.map(n => QualifiedName(n, qualifier)),
          subPatterns.map(_.map(toPattern))
        )
      case SourcePattern.VariablePattern(name)                                                       =>
        Pattern.VariablePattern(name)
      case SourcePattern.WildcardPattern(source)                                                     =>
        Pattern.WildcardPattern(source)
    }

  def convertPrecedenceDeclaration(pd: AstPrecedenceDeclaration): PrecedenceDeclaration =
    PrecedenceDeclaration(pd.relation, pd.targets)

  /** Curries lambda expressions into core format. In core, lambdas have exactly one argument, so a lambda: (a,b,c) ->
    * body, needs to be converted into: a -> b -> c -> body.
    */
  private def curryLambda(
      params: Seq[SourceLambdaParameter],
      body: Sourced[Expression]
  ): Sourced[Expression] =
    params.foldRight(body) { (param, acc) =>
      param.name.as(
        FunctionLiteral(
          param.name,
          param.typeExpression.map(convertExpression(_, typeContext = true).value).map(TypeStack.of),
          acc.map(TypeStack.of)
        )
      )
    }

  /** Curries applications into core format. f(a, b, c) becomes f(a)(b)(c). The convert function determines how each
    * argument is converted (body context for () args, type context for [] args).
    */
  private def curryApplicationWith(
      target: Sourced[Expression],
      args: Seq[Sourced[SourceExpression]],
      convert: Sourced[SourceExpression] => Sourced[Expression]
  ): Sourced[Expression] =
    args.foldLeft(target) { (acc, arg) =>
      arg.as(FunctionApplication(acc, convert(arg)))
    }
}
