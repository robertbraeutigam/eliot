{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler.Compiler(compile) where

import Engine.FactEngine
import Control.Monad.Trans.Reader
import CompilerProcessor
import qualified Logging
import Control.Exception
import Processor.ASTProcessor
import Processor.TokensProcessor
import Processor.FileProcessors
import Processor.ModuleProcessor

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = Logging.runLogger $ Logging.errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = do
   logger    <- Logging.newLogger
   (compileWithLogger paths logger) `catch` (\e -> Logging.withLogger logger $ Logging.errorMsg ("Internal compiler exception: " ++ (show (e::SomeException))))

compileWithLogger :: [String] -> Logging.Logger -> IO ()
compileWithLogger paths logger = do
   facts     <- resolveFacts liftedProcessors sourcePathFacts
   case facts of
      Just allFacts  -> Logging.withLogger logger $ Logging.debugMsg $ "Calculated facts " ++ (show (map fst allFacts))
      Nothing        -> Logging.withLogger logger $ Logging.errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathSignal s, SourcePath s)) paths
         liftedProcessors = map (liftToCompiler logger) processors
         processors = [errorProcessor, directoryWalker, fileReader, parseTokensProcessor, parseASTProcessor, parseModuleProcessor]
 
-- | Translate a fact engine IO into a compile one.
liftToCompiler :: Logging.Logger -> CompilerProcessor -> FactProcessor Signal Fact
liftToCompiler logger compilerProcessor = (withReaderT (\engine -> (logger, engine))) . compilerProcessor

-- | Error processor reads all error facts and prints them using a lock to serialize
-- all writes.
errorProcessor :: CompilerProcessor
errorProcessor (CompilerErrorFact (CompilerError fp (SourcePosition fromLine fromCol) (SourcePosition toLine toCol) msg)) = do
   source <- getCompilerFact $ SourceFileContentSignal fp
   case source of
      Just (SourceFileContent _ content) -> compilerErrorMsg fp content fromLine fromCol toLine toCol msg
      _                                  -> compileOk

errorProcessor _ = compileOk

