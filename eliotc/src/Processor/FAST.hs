{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Build the Functional AST from the AST. This represents the tree of function compositions that
 - is a function's body.
 -}

module Processor.FAST (FunctionBody(..), Expression(..), CompiledFunction(..), CompiledFunctionSignal(..), parseFASTProcessor) where

import Module
import GHC.Generics
import Data.Hashable
import Data.Tree
import Text.Read
import CompilerProcessor
import qualified Data.Map as Map
import Tokens
import qualified AST as AST
import Processor.Token

-- | A body of a function.
data FunctionBody = NativeFunction | NonNativeFunction (Tree Expression)
   deriving (Eq, Show)

-- | An expression
data Expression = NumberConstant Integer
                | FunctionApplication FunctionFQN
   deriving (Eq)

instance Show Expression where
   show (NumberConstant i) = show i
   show (FunctionApplication ffqn) = (show ffqn) ++ "()"

data CompiledFunctionSignal = CompiledFunctionSignal FunctionFQN
   deriving (Eq, Generic)

instance Hashable CompiledFunctionSignal

-- | A compiled (type-checked) correct function body, of there is no body, that's a native function
data CompiledFunction = CompiledFunction FunctionFQN FunctionBody

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

