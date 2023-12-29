{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module Processor.FASTProcessor (parseFASTProcessor) where

import Text.Read
import CompilerProcessor
import qualified Data.Map as Map
import Tokens
import qualified AST as AST
import FAST
import Module
import Processor.Token

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor v = case getTypedValue v of
   Just (FunctionCompilationUnit fname _          (AST.FunctionDefinition _ _ AST.NativeFunction))                     -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname NativeFunction)
   Just (FunctionCompilationUnit fname dictionary (AST.FunctionDefinition _ _ (AST.NonNativeFunction expressionTree))) -> do
      resolvedExpressionTree <- mapM (resolveFunctionNames dictionary) expressionTree
      case sequence resolvedExpressionTree of
         Just ret        -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname (NonNativeFunction ret))
         Nothing         -> compileOk -- Errors are generated where the expression is parsed
   _                                                                                                                   -> compileOk

resolveFunctionNames :: FunctionDictionary -> AST.Expression -> CompilerIO (Maybe Expression)
resolveFunctionNames dictionary (AST.FunctionApplication calledToken) =
   case Map.lookup (positionedTokenContent calledToken) dictionary of
      Just calledFfqn -> return $ Just (FunctionApplication calledFfqn)
      _               -> compilerErrorForTokens [calledToken] "Called function not defined." >> return Nothing
resolveFunctionNames _ (AST.NumberLiteral numberToken) =
   case readMaybe (positionedTokenContent numberToken) of
      Just number     -> return $ Just $ NumberConstant number
      _               -> compilerErrorForTokens [numberToken] "Could not parse number." >> return Nothing
