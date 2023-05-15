module Processor.TestCompiler (compileSourceCode, compileSelectFact, compileCollectFacts) where

import Control.Monad.Trans.Reader
import Engine.FactEngine
import CompilerProcessor
import Data.Maybe
import Logging

compileSourceCode :: [CompilerProcessor] -> String -> IO [(Signal, Fact)]
compileSourceCode processors code = do
   logger <- Logging.newLogger
   (fromMaybe []) <$> (resolveFacts (liftedProcessors logger) [(SourceFileRead "", SourceFileContent "" code)])
   where liftedProcessors logger = map (liftToCompiler logger) processors
            
liftToCompiler :: Logging.Logger -> CompilerProcessor -> FactProcessor Signal Fact
liftToCompiler logger compilerProcessor = (withReaderT (\engine -> (logger, engine))) . compilerProcessor

compileSelectFact :: [CompilerProcessor] -> String -> ((Signal, Fact) -> Maybe a) -> IO a
compileSelectFact processors code selector = do
   selectedAs <- compileCollectFacts processors code selector
   case selectedAs of
      []   -> error "Selection failed."
      a:_ -> return a

compileCollectFacts :: [CompilerProcessor] -> String -> ((Signal, Fact) -> Maybe a) -> IO [a]
compileCollectFacts processors code selector = do
   signalsAndFacts <- compileSourceCode processors code
   return $ catMaybes $ map selector signalsAndFacts

