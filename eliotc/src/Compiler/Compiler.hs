{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler.Compiler(compile) where

import Control.Monad
import Control.Monad.Trans.Class
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath
import System.Directory
import Engine.FactEngine
import Logging
import CompilerProcessor
import ASTProcessor
import TokensProcessor

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = do
   facts <- resolveFacts processors sourcePathFacts
   case facts of
      Just(allFacts) -> debugMsg $ "Calculated facts " ++ (show (map fst allFacts))
      Nothing        -> errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathDetected s, SourcePath s)) paths
         processors = map printErrors [directoryWalker, fileReader, parseTokensProcessor, parseASTProcessor]
         printErrors processor fact = do
            result <- processor fact
            case result of
               Right _   -> return ()
               Left errs -> return () -- TODO: print errors here

-- From here on are the processors for the compilation process

directoryWalker :: CompilerProcessor
directoryWalker (SourcePath path) = do
   isFile      <- lift $ doesFileExist path
   isDirectory <- lift $ doesDirectoryExist path
   when isFile       $ registerFact (SourceFileDetected path) (SourceFile path) 
   when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerFact . SourcePathDetected <*> SourcePath) . (path </>))
   when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
   compileOk
directoryWalker _ = compileOk

fileReader :: CompilerProcessor
fileReader (SourceFile path)
   | ".els" `isSuffixOf` path = (lift $ readFile path) >>= (registerFact (SourceFileRead path) . (SourceFileContent path)) >> compileOk
   | otherwise                = (debugMsg $ "Ignoring source file because not ending in '.els': " ++ path) >> compileOk
fileReader _ = compileOk

parseTokensProcessor :: CompilerProcessor
parseTokensProcessor (SourceFileContent path code) = case (parseTokens code) of
   Left parserError -> printCompilerError path parserError >> compileOk
   Right tokens     -> registerFact (SourceTokenized path) (SourceTokens path tokens) >> compileOk
parseTokensProcessor _ = compileOk

parseASTProcessor :: CompilerProcessor
parseASTProcessor (SourceTokens path tokens) = case parseAST tokens of
   (errors, ast) -> (sequence_ $ map (printCompilerError path) errors) >> registerFact (SourceASTCreated path) (SourceAST path ast) >> compileOk
parseASTProcessor _ = compileOk

printCompilerError :: FilePath -> CompilerError -> FactsIO Signal Fact ()
printCompilerError path compilerError = do
   source <- getFact $ SourceFileRead path
   case source of
      SourceFileContent _ content -> compilerErrorMsg path content
                                        (row $ errorFrom compilerError) (col $ errorFrom compilerError) (row $ errorTo compilerError) (col $ errorTo compilerError) (errorMessage compilerError)
      _                           -> return ()

