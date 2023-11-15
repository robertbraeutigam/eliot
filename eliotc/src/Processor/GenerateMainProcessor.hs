{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.GenerateMainProcessor (parseGenerateMain) where

import CompilerProcessor
import Module
import Generator

parseGenerateMain :: ModuleName -> TargetPlatform -> CompilerProcessor
parseGenerateMain mainModule targetPlatform Init = do
   mainFunction <- getCompilerFact $ CompiledFunctionSignal $ FunctionFQN mainModule "main"
   case mainFunction of
      Just (CompiledFunction ffqn _) -> registerCompilerFact GenerateMainSignal (GenerateMain targetPlatform ffqn)
      _                              -> errorMsg "Main function not found to compile."
parseGenerateMain _ _ _ = compileOk

