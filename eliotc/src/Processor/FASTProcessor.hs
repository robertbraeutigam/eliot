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

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor (FunctionCompilationUnit fname _          (AST.FunctionDefinition _ _ AST.NativeFunction)) =
   registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname NativeFunction)
parseFASTProcessor (FunctionCompilationUnit fname dictionary (AST.FunctionDefinition _ _ (AST.NonNativeFunction expression))) = do
   resolvedExpression <- parseExpressionTokens dictionary expression
   case resolvedExpression of
      Just expr       -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname (NonNativeFunction expr))
      Nothing         -> compileOk -- Errors are generated where the expression is parsed
parseFASTProcessor _ = compileOk

parseExpressionTokens dictionary (AST.FunctionApplication calledToken parameterExpressionTokens) =
   case Map.lookup (positionedTokenContent calledToken) dictionary of
      Just calledFfqn -> do
                            parameterExpressions <- mapM (parseExpressionTokens dictionary) parameterExpressionTokens
                            return $ (FunctionApplication calledFfqn) <$> (sequence parameterExpressions)
      _               -> compilerErrorForTokens [calledToken] "Called function not defined." >> return Nothing
parseExpressionTokens _ (AST.NumberLiteral numberToken) =
   case readMaybe (positionedTokenContent numberToken) of
      Just number     -> return $ Just $ NumberConstant number
      _               -> compilerErrorForTokens [numberToken] "Could not parse number." >> return Nothing
