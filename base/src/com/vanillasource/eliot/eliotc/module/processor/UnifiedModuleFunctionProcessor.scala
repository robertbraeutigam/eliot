package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast.FunctionDefinition
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleFunction, UnifiedModuleFunction}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

class UnifiedModuleFunctionProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case UnifiedModuleFunction.Key(ffqn) => unify(ffqn)
      case _                               => Monad[CompilerIO].unit
    }

  private def unify(ffqn: FunctionFQN): CompilerIO[Unit] =
    for {
      pathScan        <- getFactOrAbort(PathScan.Key(pathName(ffqn.moduleName)))
      allFunctions    <- pathScan.files.traverse(file => getFactOrAbort(ModuleFunction.Key(file, ffqn)).attempt.map(_.toOption)).map(_.flatten)
      unifiedFunction <- unifyFunctions(ffqn, allFunctions)
      _               <- registerFactIfClear(unifiedFunction)
    } yield ()

  private def unifyFunctions(
      ffqn: FunctionFQN,
      functions: Seq[ModuleFunction]
  ): CompilerIO[UnifiedModuleFunction] =
    if (functions.isEmpty) {
      abort[UnifiedModuleFunction]
    } else if (hasMoreImplementations(functions)) {
      compilerError(functions.head.functionDefinition.name.as("Has multiple implementations.")) *> abort[UnifiedModuleFunction]
    } else if (!hasSameSignatures(functions)) {
      compilerError(
        functions.head.functionDefinition.name.as("Has multiple different definitions.")
      ) *> abort[UnifiedModuleFunction]
    } else {
      val implementedFunction = functions.find(_.functionDefinition.body.isDefined).getOrElse(functions.head)

      UnifiedModuleFunction(
        implementedFunction.ffqn,
        implementedFunction.functionDictionary,
        implementedFunction.typeDictionary,
        implementedFunction.functionDefinition
      ).pure[CompilerIO]
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
