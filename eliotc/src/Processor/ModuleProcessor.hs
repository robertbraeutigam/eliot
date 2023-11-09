{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Parses modules and produces function names so later compiler steps can resolve function
 - names to modules.
 -}

module Processor.ModuleProcessor (parseModuleProcessor) where

import Data.Char
import Data.Maybe
import qualified Data.Map as Map
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
         functionNames     <- sequence $ map extractFunctionName (functionDefinitions ast)
         _                 <- registerCompilerFact (ModuleFunctionNamesRead mn) (ModuleFunctionNames mn (catMaybes functionNames))
         importedFunctions <- collectImportedFunctions (importStatements ast) Map.empty
         debugMsg $ "Imported functions: " ++ (show importedFunctions)
      Nothing -> compileOk
parseModuleProcessor _ = compileOk

collectImportedFunctions :: [Import] -> Map.Map String FunctionFQN -> CompilerIO (Maybe (Map.Map String FunctionFQN))
collectImportedFunctions [] fs = return $ Just fs
collectImportedFunctions (i:is) fs = do
   importedFunctionNames <- getCompilerFact (ModuleFunctionNamesRead $ toModuleName i)
   case importedFunctionNames of
      Just (ModuleFunctionNames mn names) -> if null (collisions names) then
                                                collectImportedFunctions is (Map.union fs (Map.fromList $ map (\name -> (name, FunctionFQN mn name)) names))
                                             else
                                                compilerErrorForTokens (allImportTokens i) ("Imported module imports functions that are already in scope: " ++ (show (collisions names)) ++ ".") >> return Nothing
      _                                   -> compilerErrorForTokens (allImportTokens i) ("Could not find imported module.") >> collectImportedFunctions is fs
   where
      collisions :: [String] -> [String]
      collisions names = filter (flip Map.member fs) names

toModuleName i = ModuleName (map positionedTokenContent (importPackageNames i)) (positionedTokenContent $ importModule i)

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

