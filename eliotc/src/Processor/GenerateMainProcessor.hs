{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.GenerateMainProcessor (parseGenerateMain) where

import CompilerProcessor
import Module
import Generator

parseGenerateMain :: ModuleName -> TargetPlatform -> CompilerProcessor
parseGenerateMain mainModule targetPlatform (CompiledFunction ffqn@(FunctionFQN fmn "main") _) | mainModule == fmn = registerCompilerFact GenerateMainSignal (GenerateMain targetPlatform ffqn)
parseGenerateMain _ _ _ = compileOk

