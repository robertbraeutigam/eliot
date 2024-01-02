{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Module information.
 -}

module Processor.Module (compilerErrorForFunction, parseModuleProcessor, FunctionCompilationUnitSignal(..), FunctionCompilationUnit(..), FunctionFQN(..), FunctionDictionary, ModuleName(..), ModuleFunctionNamesSignal(..), ModuleFunctionNames(..)) where

import GHC.Generics
import Data.Hashable
import Data.List(intercalate)
import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath
import CompilerProcessor
import Control.Exception.Lifted()
import Control.Monad
import Processor.AST
import Processor.Error
import Processor.Token

-- | A fully qualified module name.
data ModuleName = ModuleName [String] String
   deriving (Eq, Generic)

instance Show ModuleName where
   show (ModuleName moduleNames name) = intercalate "." (moduleNames ++ [name])

instance Hashable ModuleName

-- | Fully qualified function name.
data FunctionFQN = FunctionFQN ModuleName String
   deriving (Eq, Generic)

instance Show FunctionFQN where
   show (FunctionFQN mn fn) = (show mn) ++ "." ++ fn

instance Hashable FunctionFQN

type FunctionDictionary = Map.Map String FunctionFQN

data FunctionCompilationUnitSignal = FunctionCompilationUnitSignal FunctionFQN
   deriving (Eq, Generic)

instance Hashable FunctionCompilationUnitSignal

-- | A function ready to be compiled and type-checked
data FunctionCompilationUnit = FunctionCompilationUnit FunctionFQN FunctionDictionary FunctionDefinition

data ModuleFunctionNamesSignal = ModuleFunctionNamesSignal ModuleName
   deriving (Eq, Generic)

instance Hashable ModuleFunctionNamesSignal

-- | A list of functions in the module
data ModuleFunctionNames = ModuleFunctionNames ModuleName [String]

-- | Generate a compiler error for a fully qualified function.
compilerErrorForFunction :: FunctionFQN -> String -> CompilerIO ()
compilerErrorForFunction ffqn msg = do
   functionMaybe <- getCompilerFact (FunctionCompilationUnitSignal ffqn)
   case functionMaybe of
      Just (FunctionCompilationUnit _ _ (FunctionDefinition fname _ _)) -> compilerErrorForTokens [fname] msg
      _                                                                     -> errorMsg $ msg ++ " (Could not determine function " ++ (show ffqn) ++ " location.)"

parseModuleProcessor :: CompilerProcessor
parseModuleProcessor v = case getTypedValue v of
   Just (SourceAST path ast) -> do
      moduleNameMaybe <- calculateModuleName path ast
      case moduleNameMaybe of
         Just mn -> do
            functions         <- foldM extractFunction [] (functionDefinitions ast)
            _                 <- registerCompilerFact (ModuleFunctionNamesSignal mn) (ModuleFunctionNames mn (fst <$> functions))
            importedFunctions <- foldM (collectImportedFunctions (fst <$> functions)) Map.empty (importStatements ast)
            _                 <- forM_ functions (extractCompilationFunction mn (Map.union importedFunctions (Map.fromList $ map (\name -> (name, FunctionFQN mn name)) (fst <$> functions))))
            debugMsg $ (show mn) ++ " provides functions: " ++ (show (fst <$> functions)) ++ ", imports: " ++ (show importedFunctions)
         Nothing -> compileOk
   _                         -> compileOk

extractCompilationFunction :: ModuleName -> FunctionDictionary -> (String, FunctionDefinition) -> CompilerIO ()
extractCompilationFunction mn dictionary (fname, functionDefinition) =
   registerCompilerFact (FunctionCompilationUnitSignal $ FunctionFQN mn fname) (FunctionCompilationUnit (FunctionFQN mn fname) dictionary functionDefinition)

collectImportedFunctions :: [String] -> FunctionDictionary -> Import -> CompilerIO FunctionDictionary
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
extractFunction existingFunctions fd@(FunctionDefinition nameToken _ _) =
   if capitalized name then
      compilerErrorForTokens [nameToken] "Functions must begin with a lowercase letter or be an operator." >> return existingFunctions
   else if name `elem` (fst <$> existingFunctions) then
      compilerErrorForTokens [nameToken] "Function already declared." >> return existingFunctions
   else
      return $ (name, fd):existingFunctions
   where
      name = positionedTokenContent nameToken

calculateModuleName :: FilePath -> AST -> CompilerIO (Maybe ModuleName)
calculateModuleName path _ = 
   if capitalized fileBaseName then
      return $ Just $ ModuleName [] fileBaseName
   else
      compilerErrorForFile path "Module name must be capitalized." >> return Nothing
   where fileBaseName = takeBaseName path

capitalized []   = False
capitalized (c:_) = isUpper c

