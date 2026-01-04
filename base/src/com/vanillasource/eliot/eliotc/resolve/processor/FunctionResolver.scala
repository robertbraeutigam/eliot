package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN, UnifiedModuleFunction}
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.FunctionLiteral
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.ast
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class FunctionResolver
    extends TransformationProcessor[
      ResolvedFunction,
      UnifiedModuleFunction,
      UnifiedModuleFunction.Key,
      ResolvedFunction.Key
    ]((key: ResolvedFunction.Key) => UnifiedModuleFunction.Key(key.ffqn))
    with Logging {
  override def generateFromFact(moduleFunction: UnifiedModuleFunction): CompilerIO[ResolvedFunction] = {
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
    } yield ResolvedFunction(
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

    resolveProgram.runA(scope)
  }

  private def resolveExpression(
      expr: Sourced[ast.Expression]
  ): ScopedIO[Sourced[Expression]] =
    expr.value match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, name), args) =>
        isValueVisible(name).ifM(
          for {
            newArgs <- args.traverse(resolveExpression)
          } yield expr.as(
            newArgs.foldLeft[Expression](Expression.ParameterReference(s.as(name)))((expr, arg) =>
              Expression.FunctionApplication(s.as(expr), arg)
            )
          ),
          getFunction(name).flatMap {
            case Some(ffqn) =>
              for {
                newArgs <- args.traverse(resolveExpression)
              } yield expr.as(
                newArgs.foldLeft[Expression](Expression.ValueReference(s.as(ffqn)))((expr, arg) =>
                  Expression.FunctionApplication(s.as(expr), arg)
                )
              )
            case None       =>
              (compilerError(s.as(s"Function not defined.")) *> abort[Sourced[Expression]]).liftToScoped
          }
        )
      case ast.Expression.QualifiedFunctionApplication(
            moduleNameSrc @ Sourced(_, _, moduleNameStr),
            fnNameSrc @ Sourced(_, _, fnName),
            args
          ) =>
        for {
          newArgs <- args.traverse(resolveExpression)
        } yield {
          val moduleName = ModuleName.parse(moduleNameStr)
          val ffqn       = FunctionFQN(moduleName, fnName)
          val outline    = Sourced.outline(Seq(moduleNameSrc, fnNameSrc))
          expr.as(
            newArgs.foldLeft[Expression](Expression.ValueReference(outline.as(ffqn)))((expr, arg) =>
              Expression.FunctionApplication(outline.as(expr), arg)
            )
          )
        }
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
        } yield resolvedParameters.foldRight(resolvedBody)((arg, e) => expr.as(Expression.FunctionLiteral(arg, e)))
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, value))           =>
        expr.as(Expression.IntegerLiteral(s.as(BigInt(value)))).pure
      case ast.Expression.StringLiteral(s @ Sourced(_, _, value))            =>
        expr.as(Expression.StringLiteral(s.as(value))).pure
    }
}
