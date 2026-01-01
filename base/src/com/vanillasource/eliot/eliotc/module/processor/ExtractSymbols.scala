package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName

import java.nio.file.{Path, Paths}

object ExtractSymbols {
  def extractLocalFunctions(
      functionDefinitions: Seq[FunctionDefinition]
  ): CompilerIO[Map[String, FunctionDefinition]] =
    functionDefinitions.foldLeftM(Map.empty[String, FunctionDefinition])((acc, d) => extractLocalFunction(acc, d))

  def extractLocalTypes(definitions: Seq[DataDefinition]): CompilerIO[Map[String, DataDefinition]] =
    definitions.foldLeftM(Map.empty[String, DataDefinition])((acc, d) => extractLocalType(acc, d))

  def pathName(name: ModuleName): Path =
    (name.packages ++ Seq(name.name + ".els")).foldLeft(Paths.get(""))(_ `resolve` _)

  private def extractLocalType(
      previousTypes: Map[String, DataDefinition],
      current: DataDefinition
  ): CompilerIO[Map[String, DataDefinition]] = current.name.value match
    case ty if previousTypes.contains(ty) =>
      compilerError(current.name.as("Type was already defined in this module.")).as(previousTypes)
    case ty if !ty.charAt(0).isUpper      =>
      compilerError(current.name.as("Type name must start with upper case character."))
        .as(previousTypes)
    case ty                               => (previousTypes ++ Map((ty, current))).pure[CompilerIO]

  private def extractLocalFunction(
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  ): CompilerIO[Map[String, FunctionDefinition]] = current.name.value match
    case fn if previousFunctions.contains(fn)                                  =>
      compilerError(current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case _ if current.args.map(_.name.value).toSet.size != current.args.length =>
      val duplicateName = current.args.groupBy(_.name.value).collectFirst {
        case (_, list) if list.length > 1 => list.head
      }
      compilerError(duplicateName.get.name.as("Duplicate parameter name."))
        .as(previousFunctions)
    case fn                                                                    =>
      (previousFunctions ++ Map((fn, current))).pure[CompilerIO]
}
