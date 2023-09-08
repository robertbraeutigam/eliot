{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Parses modules and produces function names so later compiler steps can resolve function
 - names to modules.
 -}

module Processor.ModuleProcessor (parseModuleProcessor) where

import Data.Char
import Data.Maybe
import Data.Map()
import System.FilePath
import CompilerProcessor
import Control.Exception.Lifted()
import Tokens
import AST
import Module

parseModuleProcessor :: CompilerProcessor
parseModuleProcessor (SourceAST path ast) = do
   moduleName <- calculateModuleName path ast
   case moduleName of
      Just mn -> do
         functionNames <- sequence $ map extractFunctionName (functionDefinitions ast)
         debugMsg $ (show mn) ++ " has functions: " ++ (show $ catMaybes functionNames)
         registerCompilerFact (ModuleFunctionNamesRead mn) (ModuleFunctionNames mn (catMaybes functionNames))
      Nothing -> compileOk
parseModuleProcessor _ = compileOk

extractFunctionName :: FunctionDefinition -> CompilerIO (Maybe String)
extractFunctionName (FunctionDefinition [token] _) =
   return $ Just $ positionedTokenContent token
extractFunctionName (FunctionDefinition signature _) = 
   compilerErrorForTokens signature "Function signature must be only one function name (Under development)." >> return Nothing

calculateModuleName :: FilePath -> AST -> CompilerIO (Maybe ModuleName)
calculateModuleName path _ = 
   if capitalized fileBaseName then
      return $ Just $ ModuleName [] fileBaseName
   else
      compilerErrorForFile path "Module name must be capitalized." >> return Nothing
   where fileBaseName = takeBaseName path

capitalized []   = False
capitalized (c:_) = isUpper c

