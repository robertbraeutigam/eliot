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
parseFASTProcessor (FunctionCompilationUnit _ _ (FunctionDefinition _ ([PositionedToken _ _ _ (Keyword "native")]))) = compileOk -- No need to check/compile body of native function
parseFASTProcessor (FunctionCompilationUnit fname dictionary (FunctionDefinition _ [calledToken])) =
   case Map.lookup (positionedTokenContent calledToken) dictionary of
      Just calledFfqn -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname $ FunctionApplication calledFfqn)
      _               -> compilerErrorForTokens [calledToken] "Called function not defined."
parseFASTProcessor (FunctionCompilationUnit _ _ (FunctionDefinition _ ts)) = compilerErrorForTokens ts "Function body must be only one function name (Under development)."
parseFASTProcessor _ = compileOk

