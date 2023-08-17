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
import Control.Exception.Lifted
import Tokens
import AST
import FAST

parseFASTProcessor :: CompilerProcessor
parseFASTProcessor (SourceAST path ast) = do
   moduleName <- calculateModuleName path ast
   case moduleName of
      Just mn -> do
         functions  <- sequence $ map (compileFunction mn) (functionDefinitions ast)
         debugMsg ((show mn) ++ " has functions: " ++ (show (catMaybes functions)))
         registerCompilerFact (ModuleFASTCreated mn) (ModuleFAST $ Module mn (Map.fromList (catMaybes functions)))
      Nothing -> compileOk
parseFASTProcessor _ = compileOk

calculateModuleName :: FilePath -> AST -> CompilerIO (Maybe ModuleName)
calculateModuleName path _ = 
   if capitalized fileBaseName then
      return $ Just $ ModuleName [] fileBaseName
   else
      compilerErrorForFile path "Module name must be capitalized." >> return Nothing
   where fileBaseName = takeBaseName path

compileFunction :: ModuleName -> FunctionDefinition -> CompilerIO (Maybe (String, FunctionApplication))
compileFunction moduleName (FunctionDefinition signature body) = 
   if length signature > 1 then
      compilerErrorForTokens signature "Function signature must be only one function name." >> return Nothing
   else if length body > 1 then
      compilerErrorForTokens body "Body of function must be a single function name." >> return Nothing
   else do
      let functionName = positionedTokenContent $ head signature
      catch (do
         registerCompilerFact (FunctionRegistered $ FunctionName moduleName functionName) (FunctionSignaturePresent $ FunctionSignature moduleName functionName)
         return $ Just (functionName, FunctionApplication $ FunctionName (ModuleName [] "") (positionedTokenContent (head body))))
         (\e -> let _ = (e :: IOError) in ((compilerErrorForTokens signature "Duplicate function declaration.") >> (return Nothing)))
         
   -- TODO: Continue here add lookup for function, 

capitalized []   = False
capitalized (c:_) = isUpper c

