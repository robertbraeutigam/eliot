{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module Processor.FASTProcessor (parseFASTProcessor) where

import Control.Monad
import Data.Char
import qualified Data.Map as Map
import System.FilePath
import CompilerProcessor
import Tokens
import AST
import FAST

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor (SourceAST path ast) = do
   moduleName <- calculateModuleName path ast
   functions  <- sequence $ map compileFunction (functionDefinitions ast)
   registerCompilerFact (ModuleFASTCreated moduleName) (ModuleFAST $ Module moduleName (Map.fromList functions))
parseFASTProcessor _ = compileOk

calculateModuleName :: FilePath -> AST -> CompilerIO ModuleName
calculateModuleName path _ = do
   unless (capitalized fileBaseName) $ compilerErrorForFile path "Module name must be capitalized."
   return $ ModuleName [] fileBaseName
   where fileBaseName = takeBaseName path

compileFunction :: FunctionDefinition -> CompilerIO (String, FunctionApplication)
compileFunction (FunctionDefinition signature body) = do
   unless (length signature == 1) $ compilerErrorForTokens signature "Function signature must be only one function name."
   unless (length body == 1) $ compilerErrorForTokens body "Body of function must be a single functio name."
   return (positionedTokenContent (head signature), FunctionApplication $ FunctionName (ModuleName [] "") (positionedTokenContent (head body)))
   -- TODO: Get function from Module

capitalized []   = False
capitalized (c:_) = isUpper c

