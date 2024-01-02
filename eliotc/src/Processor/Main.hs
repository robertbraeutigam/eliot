{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.Main (generateMain, GenerateMainSignal(..), GenerateMain(..), TargetPlatform(..)) where

import GHC.Generics
import CompilerProcessor
import Data.Tree
import Module
import FAST
import Processor.Module
import Data.Hashable

data TargetPlatform = TargetPlatform String
   deriving (Show, Eq, Generic)

instance Hashable TargetPlatform

data GenerateMainSignal = GenerateMainSignal TargetPlatform
   deriving (Eq, Generic)

instance Hashable GenerateMainSignal

-- | Ask processors to generate for this main function and target platform.
data GenerateMain = GenerateMain TargetPlatform FunctionFQN (Tree FAST.Expression)               

generateMain :: ModuleName -> TargetPlatform -> CompilerProcessor
generateMain mainModule targetPlatform v = case getTypedValue v of
   Just (CompiledFunction ffqn@(FunctionFQN fmn "main") (NonNativeFunction expressionTree)) | mainModule == fmn -> registerCompilerFact (GenerateMainSignal targetPlatform) (GenerateMain targetPlatform ffqn expressionTree)
   Just (CompiledFunction ffqn@(FunctionFQN fmn "main") NativeFunction)                     | mainModule == fmn -> compilerErrorForFunction ffqn "Main can not be native."
   _                                                                                                     -> compileOk

