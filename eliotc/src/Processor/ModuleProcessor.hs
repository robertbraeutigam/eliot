{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Parses modules and produces function names so later compiler steps can resolve function
 - names to modules.
 -}

module Processor.ModuleProcessor (parseModuleProcessor) where

import Data.Char
import qualified Data.Map as Map
import System.FilePath
import CompilerProcessor
import Control.Exception.Lifted()
import Control.Monad
import Tokens
import AST
import Module

parseModuleProcessor :: CompilerProcessor
parseModuleProcessor (SourceAST path ast) = do
   moduleName <- calculateModuleName path ast
   case moduleName of
      Just mn -> do
         functionNames     <- foldM extractFunctionName [] (functionDefinitions ast)
         _                 <- registerCompilerFact (ModuleFunctionNamesRead mn) (ModuleFunctionNames mn functionNames)
         importedFunctions <- foldM collectImportedFunctions Map.empty (importStatements ast)
         debugMsg $ (show mn) ++ " provides functions: " ++ (show functionNames) ++ ", imports: " ++ (show importedFunctions)
      Nothing -> compileOk
parseModuleProcessor _ = compileOk

collectImportedFunctions :: Map.Map String FunctionFQN -> Import -> CompilerIO (Map.Map String FunctionFQN)
collectImportedFunctions existingFunctions i = do
   importedFunctionNames <- getCompilerFact (ModuleFunctionNamesRead $ toModuleName i)
   case importedFunctionNames of
      Just (ModuleFunctionNames mn names) -> if null (collisions names) then
                                                return $ Map.union existingFunctions (Map.fromList $ map (\name -> (name, FunctionFQN mn name)) names)
                                             else
                                                compilerErrorForTokens (allImportTokens i) ("Imported module imports functions that are already in scope: " ++ (show (collisions names)) ++ ".") >> return existingFunctions
      _                                   -> compilerErrorForTokens (allImportTokens i) ("Could not find imported module.") >> return existingFunctions
   where
      collisions :: [String] -> [String]
      collisions names = filter (flip Map.member existingFunctions) names

toModuleName i = ModuleName (map positionedTokenContent (importPackageNames i)) (positionedTokenContent $ importModule i)

extractFunctionName :: [String] -> FunctionDefinition -> CompilerIO [String]
extractFunctionName existingNames (FunctionDefinition [token] _) =
   if capitalized name then
      compilerErrorForTokens [token] "Functions must begin with a lowercase letter or be an operator." >> return existingNames
   else if name `elem` existingNames then
      compilerErrorForTokens [token] "Function already declared." >> return existingNames
   else
      return $ name:existingNames
   where
      name = positionedTokenContent token
extractFunctionName _ (FunctionDefinition signature _) = 
   compilerErrorForTokens signature "Function signature must be only one function name (Under development)." >> return []

calculateModuleName :: FilePath -> AST -> CompilerIO (Maybe ModuleName)
calculateModuleName path _ = 
   if capitalized fileBaseName then
      return $ Just $ ModuleName [] fileBaseName
   else
      compilerErrorForFile path "Module name must be capitalized." >> return Nothing
   where fileBaseName = takeBaseName path

capitalized []   = False
capitalized (c:_) = isUpper c

