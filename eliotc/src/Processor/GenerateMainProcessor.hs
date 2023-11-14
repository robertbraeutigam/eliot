{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.GenerateMainProcessor (parseGenerateMain) where

import CompilerProcessor
import Module
import Generator

parseGenerateMain :: CompilerProcessor
parseGenerateMain Init = do
   mainFunction <- getCompilerFact $ CompiledFunctionSignal $ FunctionFQN (ModuleName [] "Ni") "main" -- TODO: Get this from config
   case mainFunction of
      Just (CompiledFunction ffqn _) -> registerCompilerFact GenerateMainSignal (GenerateMain Attiny424 ffqn) -- TODO: Get target from config also
      _                              -> errorMsg "Main function not found to compile."
parseGenerateMain _ = compileOk

