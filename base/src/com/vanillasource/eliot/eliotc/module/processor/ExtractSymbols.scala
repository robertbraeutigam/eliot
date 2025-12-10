package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError

import java.nio.file.{Path, Paths}

object ExtractSymbols {
  def extractLocalFunctions[F[_]: Monad](
      functionDefinitions: Seq[FunctionDefinition]
  )(using process: CompilationProcess[F]): F[Map[String, FunctionDefinition]] =
    functionDefinitions.foldM(Map.empty[String, FunctionDefinition])((acc, d) => extractLocalFunction(acc, d))

  def extractLocalTypes[F[_]: Monad](definitions: Seq[DataDefinition])(using
      process: CompilationProcess[F]
  ): F[Map[String, DataDefinition]] =
    definitions.foldM(Map.empty[String, DataDefinition])((acc, d) => extractLocalType(acc, d))

  def pathName(name: ModuleName): Path =
    (name.packages ++ Seq(name.name + ".els")).foldLeft(Paths.get(""))(_ `resolve` _)

  private def extractLocalType[F[_]: Monad](
      previousTypes: Map[String, DataDefinition],
      current: DataDefinition
  )(using process: CompilationProcess[F]): F[Map[String, DataDefinition]] = current.name.value match
    case ty if previousTypes.contains(ty) =>
      registerCompilerError(current.name.as("Type was already defined in this module.")).as(previousTypes)
    case ty if !ty.charAt(0).isUpper      =>
      registerCompilerError(current.name.as("Type name must start with upper case character."))
        .as(previousTypes)
    case ty                               => (previousTypes ++ Map((ty, current))).pure

  private def extractLocalFunction[F[_]: Monad](
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  )(using process: CompilationProcess[F]): F[Map[String, FunctionDefinition]] = current.name.value match
    case fn if previousFunctions.contains(fn)                                  =>
      registerCompilerError(current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case _ if current.args.map(_.name.value).toSet.size != current.args.length =>
      val duplicateName = current.args.groupBy(_.name.value).collectFirst {
        case (_, list) if list.length > 1 => list.head
      }
      registerCompilerError(duplicateName.get.name.as("Duplicate parameter name."))
        .as(previousFunctions)
    case fn                                                                    =>
      (previousFunctions ++ Map((fn, current))).pure
}
