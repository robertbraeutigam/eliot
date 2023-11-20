{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module Processor.FASTProcessor (parseFASTProcessor) where

import CompilerProcessor
import qualified Data.Map as Map
import Tokens
import AST
import FAST

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor (FunctionCompilationUnit fname _ (FunctionDefinition _ _ NativeFunctionToken)) =
   registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname NativeFunction)
parseFASTProcessor (FunctionCompilationUnit fname dictionary (FunctionDefinition _ _ (FunctionApplicationTokens calledToken))) =
   case Map.lookup (positionedTokenContent calledToken) dictionary of
      Just calledFfqn -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname $ FunctionApplication calledFfqn)
      _               -> compilerErrorForTokens [calledToken] "Called function not defined."
parseFASTProcessor _ = compileOk

