{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler.Compiler(compile) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Engine.FactEngine
import Logging
import CompilerProcessor
import Processor.ASTProcessor
import Processor.TokensProcessor
import Processor.FileProcessors

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

-- | Error processor reads all error facts and prints them using a lock to serialize
-- all writes.
errorProcessor :: MVar () -> CompilerProcessor
errorProcessor lock (CompilerErrorFact (CompilerError fp (SourcePosition fromLine fromCol) (SourcePosition toLine toCol) msg)) = do
   source <- getFact $ SourceFileRead fp
   case source of
      SourceFileContent _ content -> lift $ withMVar lock $ const $ compilerErrorMsg fp content fromLine fromCol toLine toCol msg
      _                           -> return ()

errorProcessor _ _ = compileOk

