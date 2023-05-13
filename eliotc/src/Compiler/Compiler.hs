{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler.Compiler(compile) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath
import System.Directory
import Engine.FactEngine
import Logging
import CompilerProcessor
import Processor.ASTProcessor
import Processor.TokensProcessor

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = do
   printLock <- newMVar ()
   facts     <- resolveFacts (processors printLock) sourcePathFacts
   case facts of
      Just(allFacts) -> debugMsg $ "Calculated facts " ++ (show (map fst allFacts))
      Nothing        -> errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathDetected s, SourcePath s)) paths
         processors printLock = [errorProcessor printLock, directoryWalker, fileReader, parseTokensProcessor, parseASTProcessor]

-- From here on are the processors for the compilation process

directoryWalker :: CompilerProcessor
directoryWalker (SourcePath path) = do
   isFile      <- lift $ doesFileExist path
   isDirectory <- lift $ doesDirectoryExist path
   when isFile       $ registerCompilerFact (SourceFileDetected path) (SourceFile path) 
   when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerCompilerFact . SourcePathDetected <*> SourcePath) . (path </>))
   when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
   compileOk
directoryWalker _ = compileOk

fileReader :: CompilerProcessor
fileReader (SourceFile path)
   | ".els" `isSuffixOf` path = (lift $ readFile path) >>= (registerCompilerFact (SourceFileRead path) . (SourceFileContent path)) >> compileOk
   | otherwise                = (debugMsg $ "Ignoring source file because not ending in '.els': " ++ path) >> compileOk
fileReader _ = compileOk

parseTokensProcessor :: CompilerProcessor
parseTokensProcessor (SourceFileContent path code) = case (parseTokens path code) of
   Left parserError -> compilerError parserError >> compileOk
   Right tokens     -> registerCompilerFact (SourceTokenized path) (SourceTokens path tokens) >> compileOk
parseTokensProcessor _ = compileOk

parseASTProcessor :: CompilerProcessor
parseASTProcessor (SourceTokens path tokens) = case parseAST path tokens of
   (errors, ast) -> (mapM_ compilerError errors) >> registerCompilerFact (SourceASTCreated path) (SourceAST path ast) >> compileOk
parseASTProcessor _ = compileOk

-- | Error processor reads all error facts and prints them using a lock to serialize
-- all writes.
errorProcessor :: MVar () -> CompilerProcessor
errorProcessor lock (CompilerErrorFact (CompilerError fp (SourcePosition fromLine fromCol) (SourcePosition toLine toCol) msg)) = do
   source <- getFact $ SourceFileRead fp
   case source of
      SourceFileContent _ content -> lift $ withMVar lock $ const $ compilerErrorMsg fp content fromLine fromCol toLine toCol msg
      _                           -> return ()

errorProcessor _ _ = compileOk

