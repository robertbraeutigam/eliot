{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.GenerateMainProcessor (parseGenerateMain) where

import CompilerProcessor
import Module
import Generator
import FAST

-- TODO: Just generate the compile on Init?
parseGenerateMain :: ModuleName -> TargetPlatform -> CompilerProcessor
parseGenerateMain mainModule targetPlatform v = case getTypedValue v of
   Just (CompiledFunction ffqn@(FunctionFQN fmn "main") (NonNativeFunction expressionTree)) | mainModule == fmn -> registerCompilerFact (GenerateMainSignal targetPlatform) (GenerateMain targetPlatform ffqn expressionTree)
   Just (CompiledFunction ffqn@(FunctionFQN fmn "main") NativeFunction)                     | mainModule == fmn -> compilerErrorForFunction ffqn "Main can not be native."
   _                                                                                                     -> compileOk

