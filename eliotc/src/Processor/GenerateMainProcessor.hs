{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.GenerateMainProcessor (parseGenerateMain) where

import CompilerProcessor
import Module
import Generator
import FAST

parseGenerateMain :: ModuleName -> TargetPlatform -> CompilerProcessor
parseGenerateMain mainModule targetPlatform (CompiledFunction ffqn@(FunctionFQN fmn "main") (NonNativeFunction expressionTree)) | mainModule == fmn = registerCompilerFact (GenerateMainSignal targetPlatform) (GenerateMain targetPlatform ffqn expressionTree)
parseGenerateMain mainModule _              (CompiledFunction ffqn@(FunctionFQN fmn "main") NativeFunction)                     | mainModule == fmn = compilerErrorForFunction ffqn "Main can not be native."
parseGenerateMain _ _ _ = compileOk

