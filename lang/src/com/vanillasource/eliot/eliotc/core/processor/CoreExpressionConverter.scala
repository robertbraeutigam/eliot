package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  GenericParameter,
  ArgumentDefinition as SourceArgument,
  Expression as SourceExpression,
  LambdaParameterDefinition as SourceLambdaParameter,
  PrecedenceDeclaration as AstPrecedenceDeclaration,
  QualifiedName as AstQualifiedName,
  Qualifier as AstQualifier,
  Pattern as SourcePattern
}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.*
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
        if (isUpper && args.isEmpty && typeContext) {
          val base = expr.as(NamedValueReference(fnName.map(n => QualifiedName(n, Qualifier.Type)), moduleName))
          curryApplicationWith(base, genericArgs, convertExpression(_, typeContext = true))
        } else if (isUpper && args.isEmpty && genericArgs.nonEmpty) {
          val base = expr.as(NamedValueReference(fnName.map(n => QualifiedName(n, Qualifier.Type)), moduleName))
          curryApplicationWith(base, genericArgs, convertExpression(_, typeContext = true))
        } else {
          curryApplicationWith(
            expr.as(
              NamedValueReference(
                fnName.map(n => QualifiedName(n, Qualifier.Default)),
                moduleName,
                genericArgs.map(convertExpression(_, typeContext = true))
              )
            ),
            args,
            convertExpression(_, typeContext = false)
          )
        }
      case SourceExpression.FunctionLiteral(params, body)                              =>
        curryLambda(params, convertExpression(body, typeContext))
      case SourceExpression.IntegerLiteral(lit)                                        =>
        expr.as(IntegerLiteral(lit))
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
            functionRef.map(TypeStack.of),
            argType.map(TypeStack.of)
          )
        )
        arg.name.as(
          FunctionApplication(
            withArgType.map(TypeStack.of),
            acc.map(TypeStack.of)
          )
        )
    }
    genericParams.foldRight[Sourced[Expression]](withArgs) { (param, acc) =>
      val kindType = convertExpression(param.typeRestriction, typeContext = true)
      param.name.as(FunctionLiteral(param.name, Some(TypeStack.of(kindType.value)), acc.map(TypeStack.of)))
    }
  }

  /** Converts the body into core expression and embeds it as a lambda with the "function" parameters.
    */
  def buildCurriedBody(
      args: Seq[SourceArgument],
      value: Sourced[SourceExpression]
  ): Sourced[Expression] =
    args.foldRight(convertExpression(value, typeContext = false)) { (arg, acc) =>
      arg.name.as(
        FunctionLiteral(
          arg.name,
          Some(TypeStack.of(convertExpression(arg.typeExpression, typeContext = true).value)),
          acc.map(TypeStack.of)
        )
      )
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
      case SourcePattern.VariablePattern(name)                                       =>
        Pattern.VariablePattern(name)
      case SourcePattern.WildcardPattern(source)                                     =>
        Pattern.WildcardPattern(source)
    }

  def convertQualifiedName(name: Sourced[AstQualifiedName]): Sourced[QualifiedName] =
    name.map(n => QualifiedName(n.name, convertQualifier(n.qualifier)))

  def convertQualifier(qualifier: AstQualifier): Qualifier =
    qualifier match {
      case AstQualifier.Default                           => Qualifier.Default
      case AstQualifier.Type                              => Qualifier.Type
      case AstQualifier.Ability(n)                        => Qualifier.Ability(n)
      case AstQualifier.AbilityImplementation(n, pattern) =>
        Qualifier.AbilityImplementation(
          n,
          pattern.map(convertExpression(_, typeContext = true).value)
        )
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
      arg.as(FunctionApplication(acc.map(TypeStack.of), convert(arg).map(TypeStack.of)))
    }
}
