package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleData, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, ast}

class FunctionResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ModuleFunction(
          ffqn,
          functionDictionary,
          typeDictionary,
          ast.FunctionDefinition(name, genericParameters, args, typeDefinition, body)
        ) =>
      process(
        ffqn,
        functionDictionary,
        typeDictionary,
        name,
        genericParameters,
        args,
        typeDefinition,
        body
      ).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDictionary: Map[String, FunctionFQN],
      typeDictionary: Map[String, TypeFQN],
      name: Sourced[String],
      genericParameters: Seq[ast.GenericParameter],
      args: Seq[ast.ArgumentDefinition],
      typeReference: ast.TypeReference,
      body: Option[ast.Expression]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val scope = ResolverScope(
      functionDictionary,
      typeDictionary,
      genericParameters.map(gp => gp.name.value -> gp).toMap,
      args.map(arg => arg.name.value -> arg).toMap
    )

    val resolveProgram = for {
      resolvedBody              <- body.traverse(resolveExpression)
      returnType                <- resolveType(typeReference)
      argumentTypes             <- args.map(_.typeReference).traverse(resolveType)
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(resolveType)
            .map(resolvedGenericTypes => UniversalGenericParameter(genericParameter.name, resolvedGenericTypes))
        )
      _                         <-
        process
          .registerFact(
            ResolvedFunction(
              ffqn,
              FunctionDefinition(
                name,
                resolvedGenericParameters,
                args
                  .zip(argumentTypes)
                  .map((argDef, argType) => ArgumentDefinition(argDef.name, argType)),
                returnType,
                resolvedBody
              )
            )
          )
          .liftToCompilationIO
          .liftToScoped
    } yield ()

    resolveProgram.runS(scope).void
  }

  private def resolveType(reference: ast.TypeReference)(using process: CompilationProcess): ScopedIO[TypeReference] =
    for {
      resolvedGenericParameters <- reference.genericParameters.traverse(resolveType)
      resolvedType              <- getGenericParameter(reference.typeName.value).flatMap {
                                     case Some(genericParameter) =>
                                       if (genericParameter.genericParameters.length =!= resolvedGenericParameters.length) {
                                         compilerAbort(
                                           reference.typeName.as("Incorrect number of generic parameters for type.")
                                         ).liftToScoped
                                       } else {
                                         GenericTypeReference(genericParameter.name, resolvedGenericParameters)
                                           .pure[ScopedIO]
                                       }
                                     case None                   =>
                                       getType(reference.typeName.value).flatMap {
                                         case Some(typeFqn) =>
                                           for {
                                             dataDefinition <-
                                               process.getFact(ModuleData.Key(typeFqn)).liftOptionToCompilationIO.liftToScoped
                                             _              <-
                                               compilerAbort(
                                                 reference.typeName.as("Incorrect number of generic parameters for type.")
                                               ).liftToScoped.whenA(
                                                 dataDefinition.dataDefinition.genericParameters.length =!= resolvedGenericParameters.length
                                               )
                                           } yield DirectTypeReference(reference.typeName.as(typeFqn), resolvedGenericParameters)
                                         case None          => compilerAbort(reference.typeName.as("Type not defined.")).liftToScoped
                                       }
                                   }
    } yield resolvedType

  private def resolveExpression(expr: ast.Expression)(using process: CompilationProcess): ScopedIO[Expression] =
    expr match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, name), args) =>
        isValueVisible(name).ifM(
          Expression.ParameterReference(s.as(name)).pure,
          getFunction(name).flatMap {
            case Some(ffqn) =>
              for {
                newArgs <- args.traverse(resolveExpression)
              } yield Expression.FunctionApplication(s.as(ffqn), newArgs)
            case None       => compilerAbort(s.as(s"Function not defined.")).liftToScoped
          }
        )
      case ast.Expression.FunctionLiteral(parameters, body)                  =>
        for {
          resolvedParameters <-
            parameters
              .traverse(arg =>
                resolveType(arg.typeReference).map(resolvedType => ArgumentDefinition(arg.name, resolvedType))
              )
          _                  <- parameters.traverse(addVisibleValue)
          resolvedExpression <- resolveExpression(body)
        } yield Expression.FunctionLiteral(resolvedParameters, resolvedExpression)
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, value))           =>
        Expression.IntegerLiteral(s.as(BigInt(value))).pure
    }
}
