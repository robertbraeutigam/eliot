module Processor.TestCompiler (compileSelectFact, compileCollectFacts) where

import Control.Monad.Trans.Reader
import Engine.DynamicFactEngine
import CompilerProcessor
import Data.Maybe
import Data.Dynamic
import Logging
import Processor.FileProcessors

compileSourceCode :: [CompilerProcessor] -> [(String, String)] -> IO [(DynamicKey, DynamicValue)]
compileSourceCode processors files = do
   logger <- Logging.newLogger
   (fromMaybe []) <$> (resolveFacts (liftedProcessors logger) $ map (\(filename, code) -> (toDynKey $ SourceFileContentSignal filename, toDynValue $ SourceFileContent filename code)) files)
   where liftedProcessors logger = map (liftToCompiler logger) processors
            
liftToCompiler :: Logging.Logger -> CompilerProcessor -> (DynamicValue -> DynamicFactsIO ())
liftToCompiler logger compilerProcessor = withReaderT (\engine -> (logger, engine)) . compilerProcessor

compileSelectFact :: (Typeable s, Typeable f) => [CompilerProcessor] -> [(String, String)] -> ((s, f) -> Maybe a) -> IO a
compileSelectFact processors files selector = do
   selectedAs <- compileCollectFacts processors files selector
   case selectedAs of
      []   -> error "Selection failed."
      a:_ -> return a

compileCollectFacts :: (Typeable s, Typeable f) => [CompilerProcessor] -> [(String, String)] -> ((s, f) -> Maybe a) -> IO [a]
compileCollectFacts processors files selector = do
   signalsAndFacts <- compileSourceCode processors files
   return $ catMaybes $ map selector $ catMaybes $ map transformDynamics signalsAndFacts
   where transformDynamics (DynamicKey k _ _, DynamicValue v) = do
            tk <- fromDynamic k
            tv <- fromDynamic v
            return (tk, tv)

