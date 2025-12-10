package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.OptionT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN, UnifiedModuleFunction}
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.FunctionLiteral
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.source.error.CompilationF.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, ast}
import cats.Monad
import cats.effect.Sync
import cats.effect.std.Console

class FunctionResolver[F[_]: {Sync, Console}]
    extends OneToOneProcessor((key: ResolvedFunction.Key) => UnifiedModuleFunction.Key(key.ffqn))
    with Logging {
  override def generateFromFact(
      moduleFunction: UnifiedModuleFunction
  )(using process: CompilationProcess[F]): F[Unit] = {
    val args              = moduleFunction.functionDefinition.args
    val body              = moduleFunction.functionDefinition.body
    val genericParameters = moduleFunction.functionDefinition.genericParameters
    val typeReference     = moduleFunction.functionDefinition.typeDefinition
    val name              = moduleFunction.functionDefinition.name

    val scope = ResolverScope(
      moduleFunction.functionDictionary,
      moduleFunction.typeDictionary,
      genericParameters.map(gp => gp.name.value -> gp).toMap,
      args.map(arg => arg.name.value -> arg).toMap
    )

    val resolveProgram = for {
      resolvedBodyMaybe         <- body.traverse(resolveExpression)
      returnType                <- TypeResolver.resolveType(typeReference)
      argumentTypes             <- args.map(_.typeReference).traverse(TypeResolver.resolveType)
      argumentDefinitions        = args.zip(argumentTypes).map((argDef, argType) => ArgumentDefinition(argDef.name, argType))
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(TypeResolver.resolveType)
            .map(resolvedGenericTypes => UniversalGenericParameter(genericParameter.name, resolvedGenericTypes))
        )
      _                         <- debug[F](s"Resolved ${moduleFunction.ffqn.show}").liftToCompilation.liftToScoped
      _                         <-
        process
          .registerFact(
            ResolvedFunction(
              moduleFunction.ffqn,
              FunctionDefinition(
                name,
                resolvedGenericParameters,
                // Unroll return type into Function[A, Function[B, ...]]
                argumentDefinitions.foldRight(returnType)((arg, typ) =>
                  DirectTypeReference(
                    typeReference.typeName.as(TypeFQN.systemFunctionType),
                    Seq(arg.typeReference, typ)
                  )
                ),
                // Unroll body to use function literals: arg1 -> arg2 -> ... -> body
                resolvedBodyMaybe.map(resolvedBody =>
                  argumentDefinitions.foldRight(resolvedBody)((arg, expr) => expr.as(FunctionLiteral(arg, expr)))
                )
              )
            )
          )
          .liftToCompilation
          .liftToScoped
    } yield ()

    resolveProgram.runS(scope).void.runCompilation_()
  }

  private def resolveExpression(
      expr: Sourced[ast.Expression]
  )(using process: CompilationProcess[F]): ScopedF[F, Sourced[Expression]] =
    expr.value match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, name), args) =>
        isValueVisible(name).ifM(
          expr.as(Expression.ParameterReference(s.as(name))).pure,
          getFunction(name).flatMap {
            case Some(ffqn) =>
              for {
                newArgs <- args.traverse(resolveExpression)
              } yield expr.as(
                newArgs.foldRight[Expression](Expression.ValueReference(s.as(ffqn)))((arg, expr) =>
                  Expression.FunctionApplication(s.as(expr), arg)
                )
              )
            case None       => compilerAbort(s.as(s"Function not defined.")).liftToScoped
          }
        )
      case ast.Expression.FunctionLiteral(parameters, body)                  =>
        for {
          resolvedParameters <-
            parameters
              .traverse(arg =>
                TypeResolver
                  .resolveType(arg.typeReference)
                  .map(resolvedType => ArgumentDefinition(arg.name, resolvedType))
              )
          _                  <- parameters.traverse(addVisibleValue)
          resolvedBody       <- resolveExpression(body)
        } yield resolvedParameters.foldRight(resolvedBody)((arg, expr) =>
          expr.as(Expression.FunctionLiteral(arg, expr))
        )
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, value))           =>
        expr.as(Expression.IntegerLiteral(s.as(BigInt(value)))).pure
      case ast.Expression.StringLiteral(s @ Sourced(_, _, value))            =>
        expr.as(Expression.StringLiteral(s.as(value))).pure
    }
}
