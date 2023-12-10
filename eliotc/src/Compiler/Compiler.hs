{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler.Compiler(compile) where

import Engine.DynamicFactEngine
import Control.Monad.Trans.Reader
import CompilerProcessor
import qualified Logging
import Control.Exception
import Processor.ASTProcessor
import Processor.TokensProcessor
import Processor.FileProcessors
import Processor.ModuleProcessor
import Processor.FASTProcessor
import Processor.GenerateMainProcessor
import Processor.AVRGeneratorProcessor
import Processor.BinaryOutputProcessor
import Module
import Generator

-- | Run the compiler on the given source paths.
compile :: ModuleName -> TargetPlatform -> [String] -> IO ()
compile _ _ [] = Logging.runLogger $ Logging.errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile architecture mainModule paths = do
   logger    <- Logging.newLogger
   (compileWithLogger architecture mainModule paths logger) `catch` (\e -> Logging.withLogger logger $ Logging.errorMsg ("Internal compiler exception: " ++ (show (e::SomeException))))

compileWithLogger :: ModuleName -> TargetPlatform -> [String] -> Logging.Logger -> IO ()
compileWithLogger mainModule architecture paths logger = do
   facts          <- resolveFacts liftedProcessors ((toDynKey InitSignal, toDynValue Init):sourcePathFacts)
   case facts of
      Just allFacts  -> Logging.withLogger logger $ Logging.debugMsg $ "Calculated " ++ (show (length allFacts)) ++ " facts."
      Nothing        -> Logging.withLogger logger $ Logging.errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (toDynKey $ SourcePathSignal s, toDynValue $ SourcePath s)) paths
         liftedProcessors = map (liftToCompiler logger) processors
         processors = [errorProcessor, directoryWalker, fileReader, parseTokensProcessor, parseASTProcessor, parseModuleProcessor, parseFASTProcessor, parseGenerateMain mainModule architecture, parseAVRGenerate, writeOutputBinary]
 
-- | Translate a fact engine IO into a compile one.
liftToCompiler :: Logging.Logger -> CompilerProcessor -> (DynamicValue -> DynamicFactsIO ())
liftToCompiler logger compilerProcessor = withReaderT (\engine -> (logger, engine)) . compilerProcessor

-- | Error processor reads all error facts and prints them using a lock to serialize
-- all writes.
errorProcessor :: CompilerProcessor
errorProcessor v = case getTypedValue v of
   Just ((CompilerErrorFact (CompilerError fp (SourcePosition fromLine fromCol) (SourcePosition toLine toCol) msg))) -> do
      source <- getCompilerFact $ SourceFileContentSignal fp
      case source of
         Just (SourceFileContent _ content) -> compilerErrorMsg fp content fromLine fromCol toLine toCol msg
         _                                  -> compileOk
   _                                                                                                                 -> compileOk


