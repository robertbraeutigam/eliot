module Processor.TestCompiler (compileSourceCode, compileSelectFact, compileCollectFacts) where

import Control.Monad.Trans.Reader
import Engine.FactEngine
import CompilerProcessor
import Data.Maybe
import Logging

compileSourceCode :: [CompilerProcessor] -> [(String, String)] -> IO [(Signal, Fact)]
compileSourceCode processors files = do
   logger <- Logging.newLogger
   (fromMaybe []) <$> (resolveFacts (liftedProcessors logger) $ map (\(filename, code) -> (SourceFileRead filename, SourceFileContent filename code)) files)
   where liftedProcessors logger = map (liftToCompiler logger) processors
            
liftToCompiler :: Logging.Logger -> CompilerProcessor -> FactProcessor Signal Fact
liftToCompiler logger compilerProcessor = (withReaderT (\engine -> (logger, engine))) . compilerProcessor

compileSelectFact :: [CompilerProcessor] -> [(String, String)] -> ((Signal, Fact) -> Maybe a) -> IO a
compileSelectFact processors files selector = do
   selectedAs <- compileCollectFacts processors files selector
   case selectedAs of
      []   -> error "Selection failed."
      a:_ -> return a

compileCollectFacts :: [CompilerProcessor] -> [(String, String)] -> ((Signal, Fact) -> Maybe a) -> IO [a]
compileCollectFacts processors files selector = do
   signalsAndFacts <- compileSourceCode processors files
   return $ catMaybes $ map selector signalsAndFacts

