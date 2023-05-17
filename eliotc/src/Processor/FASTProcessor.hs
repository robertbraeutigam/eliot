{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module Processor.FASTProcessor (parseFASTProcessor) where

import Data.Char
import Data.Maybe
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
   case moduleName of
      Just mn -> registerCompilerFact (ModuleFASTCreated mn) (ModuleFAST $ Module mn (Map.fromList (catMaybes functions)))
      Nothing -> compileOk
parseFASTProcessor _ = compileOk

calculateModuleName :: FilePath -> AST -> CompilerIO (Maybe ModuleName)
calculateModuleName path _ = 
   if capitalized fileBaseName then
      return $ Just $ ModuleName [] fileBaseName
   else
      compilerErrorForFile path "Module name must be capitalized." >> return Nothing
   where fileBaseName = takeBaseName path

compileFunction :: FunctionDefinition -> CompilerIO (Maybe (String, FunctionApplication))
compileFunction (FunctionDefinition signature body) = 
   if length signature > 1 then
      compilerErrorForTokens signature "Function signature must be only one function name." >> return Nothing
   else if length body > 1 then
      compilerErrorForTokens body "Body of function must be a single function name." >> return Nothing
   else
      return $ Just (positionedTokenContent (head signature), FunctionApplication $ FunctionName (ModuleName [] "") (positionedTokenContent (head body)))
   -- TODO: Continue here add lookup for function, 

capitalized []   = False
capitalized (c:_) = isUpper c

