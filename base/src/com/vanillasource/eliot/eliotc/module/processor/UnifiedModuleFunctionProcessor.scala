package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.data.OptionT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleFunction, UnifiedModuleFunction}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class UnifiedModuleFunctionProcessor[F[_]: Monad] extends CompilerProcessor[F] {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess[F]): F[Unit] =
    factKey match {
      case UnifiedModuleFunction.Key(ffqn) => unify(ffqn).getOrUnit
      case _                               => Monad[F].unit
    }

  private def unify(ffqn: FunctionFQN)(using process: CompilationProcess[F]): OptionT[F, Unit] =
    for {
      files           <- process.getFact(PathScan.Key(pathName(ffqn.moduleName))).toOptionT.map(_.files)
      allFunctions    <- files.traverse(file => process.getFact(ModuleFunction.Key(file, ffqn))).map(_.flatten).liftOptionT
      unifiedFunction <- unifyFunctions(allFunctions)
      _               <- process.registerFact(unifiedFunction).liftOptionT
    } yield ()

  private def unifyFunctions(
      functions: Seq[ModuleFunction]
  )(using CompilationProcess[F]): OptionT[F, UnifiedModuleFunction] = {
    if (functions.isEmpty) {
      OptionT.none
    } else if (hasMoreImplementations(functions)) {
      registerCompilerError(functions.head.functionDefinition.name.as("Has multiple implementations.")).liftOptionTNone
    } else if (!hasSameSignatures(functions)) {
      registerCompilerError(
        functions.head.functionDefinition.name.as("Has multiple different definitions.")
      ).liftOptionTNone
    } else {
      val implementedFunction = functions.find(_.functionDefinition.body.isDefined).getOrElse(functions.head)

      UnifiedModuleFunction(
        implementedFunction.ffqn,
        implementedFunction.functionDictionary,
        implementedFunction.typeDictionary,
        implementedFunction.functionDefinition
      )
        .pure[F]
        .liftOptionT
    }
  }

  private def hasMoreImplementations(functions: Seq[ModuleFunction]): Boolean =
    functions.count(_.functionDefinition.body.isDefined) > 1

  private def hasSameSignatures(functions: Seq[ModuleFunction]): Boolean = {
    val first = functions.head

    functions.tail.forall(fn =>
      FunctionDefinition.signatureEquality.eqv(first.functionDefinition, fn.functionDefinition)
    )
  }
}
