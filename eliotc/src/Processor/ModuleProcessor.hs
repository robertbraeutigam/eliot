{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Parses modules and produces function names so later compiler steps can resolve function
 - names to modules.
 -}

module Processor.ModuleProcessor (parseModuleProcessor) where

import Data.Char
import qualified Data.List as List
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
         functions         <- foldM extractFunction [] (functionDefinitions ast)
         _                 <- registerCompilerFact (ModuleFunctionNamesSignal mn) (ModuleFunctionNames mn (fst <$> functions))
         importedFunctions <- foldM (collectImportedFunctions (fst <$> functions)) Map.empty (importStatements ast)
         _                 <- forM_ functions (extractCompilationFunction mn (Map.union importedFunctions (Map.fromList $ map (\name -> (name, FunctionFQN mn name)) (fst <$> functions))))
         debugMsg $ (show mn) ++ " provides functions: " ++ (show (fst <$> functions)) ++ ", imports: " ++ (show importedFunctions)
      Nothing -> compileOk
parseModuleProcessor _ = compileOk

extractCompilationFunction :: ModuleName -> Map.Map String FunctionFQN -> (String, FunctionDefinition) -> CompilerIO ()
extractCompilationFunction mn dictionary (functionName, functionDefinition) =
   registerCompilerFact (FunctionCompilationUnitSignal $ FunctionFQN mn functionName) (FunctionCompilationUnit (FunctionFQN mn functionName) dictionary functionDefinition)

collectImportedFunctions :: [String] -> Map.Map String FunctionFQN -> Import -> CompilerIO (Map.Map String FunctionFQN)
collectImportedFunctions existingNames existingFunctions i = do
   importedFunctionNames <- getCompilerFact (ModuleFunctionNamesSignal $ toModuleName i)
   case importedFunctionNames of
      Just (ModuleFunctionNames mn names) -> if null (collisions names) then
                                                return $ Map.union existingFunctions (Map.fromList $ map (\name -> (name, FunctionFQN mn name)) names)
                                             else
                                                compilerErrorForTokens (allImportTokens i) ("Imported module imports functions that are already in scope: " ++ (show (collisions names)) ++ ".") >> return existingFunctions
      _                                   -> compilerErrorForTokens (allImportTokens i) ("Could not find imported module.") >> return existingFunctions
   where
      collisions :: [String] -> [String]
      collisions names = (filter (flip Map.member existingFunctions) names) ++ (List.intersect names existingNames)

toModuleName i = ModuleName (map positionedTokenContent (importPackageNames i)) (positionedTokenContent $ importModule i)

extractFunction :: [(String, FunctionDefinition)] -> FunctionDefinition -> CompilerIO [(String, FunctionDefinition)]
extractFunction existingFunctions fd@(FunctionDefinition [token] _) =
   if capitalized name then
      compilerErrorForTokens [token] "Functions must begin with a lowercase letter or be an operator." >> return existingFunctions
   else if name `elem` (fst <$> existingFunctions) then
      compilerErrorForTokens [token] "Function already declared." >> return existingFunctions
   else
      return $ (name, fd):existingFunctions
   where
      name = positionedTokenContent token
extractFunction _ (FunctionDefinition signature _) = 
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

