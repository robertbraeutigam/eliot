module Processor.Module (compilerErrorForFunction) where

import AST
import Module
import CompilerProcessor
import Processor.Token

-- | Generate a compiler error for a fully qualified function.
compilerErrorForFunction :: FunctionFQN -> String -> CompilerIO ()
compilerErrorForFunction ffqn msg = do
   functionMaybe <- getCompilerFact (FunctionCompilationUnitSignal ffqn)
   case functionMaybe of
      Just (FunctionCompilationUnit _ _ (AST.FunctionDefinition fname _ _)) -> compilerErrorForTokens [fname] msg
      _                                                                     -> errorMsg $ msg ++ " (Could not determine function " ++ (show ffqn) ++ " location.)"

