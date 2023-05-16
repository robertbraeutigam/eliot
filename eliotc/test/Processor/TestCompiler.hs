module Processor.TestCompiler (compileSourceCode, compileSelectFact, compileCollectFacts) where

import Control.Monad.Trans.Reader
import Engine.FactEngine
import CompilerProcessor
import Data.Maybe
import Logging

compileSourceCode :: [CompilerProcessor] -> String -> String -> IO [(Signal, Fact)]
compileSourceCode processors filename code = do
   logger <- Logging.newLogger
   (fromMaybe []) <$> (resolveFacts (liftedProcessors logger) [(SourceFileRead filename, SourceFileContent filename code)])
   where liftedProcessors logger = map (liftToCompiler logger) processors
            
liftToCompiler :: Logging.Logger -> CompilerProcessor -> FactProcessor Signal Fact
liftToCompiler logger compilerProcessor = (withReaderT (\engine -> (logger, engine))) . compilerProcessor

compileSelectFact :: [CompilerProcessor] -> String -> String -> ((Signal, Fact) -> Maybe a) -> IO a
compileSelectFact processors filename code selector = do
   selectedAs <- compileCollectFacts processors filename code selector
   case selectedAs of
      []   -> error "Selection failed."
      a:_ -> return a

compileCollectFacts :: [CompilerProcessor] -> String -> String -> ((Signal, Fact) -> Maybe a) -> IO [a]
compileCollectFacts processors filename code selector = do
   signalsAndFacts <- compileSourceCode processors filename code
   return $ catMaybes $ map selector signalsAndFacts

