{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module Processor.FASTProcessor (parseFASTProcessor) where

import Text.Read
import CompilerProcessor
import qualified Data.Map as Map
import Tokens
import AST
import FAST

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor (FunctionCompilationUnit fname _          (FunctionDefinition _ _ NativeFunctionToken)) =
   registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname NativeFunction)
parseFASTProcessor (FunctionCompilationUnit fname dictionary (FunctionDefinition _ _ (Expression expressionTokens))) = do
   expression <- parseExpressionTokens dictionary expressionTokens
   case expression of
      Just exp        -> registerCompilerFact (CompiledFunctionSignal fname) (CompiledFunction fname (FunctionExpression exp))
      Nothing         -> compileOk -- Errors are generated where the expression is parsed
parseFASTProcessor _ = compileOk

parseExpressionTokens dictionary (FunctionApplicationTokens calledToken parameterExpressionTokens) =
   case Map.lookup (positionedTokenContent calledToken) dictionary of
      Just calledFfqn -> do
                            parameterExpressions <- mapM (parseExpressionTokens dictionary) parameterExpressionTokens
                            return $ (FunctionApplication calledFfqn) <$> (sequence parameterExpressions)
      _               -> compilerErrorForTokens [calledToken] "Called function not defined." >> return Nothing
parseExpressionTokens dictionary (NumberLiteralToken numberToken) =
   case readMaybe (positionedTokenContent numberToken) of
      Just number     -> return $ Just $ NumberConstant number
      _               -> compilerErrorForTokens [numberToken] "Could not parse number." >> return Nothing
