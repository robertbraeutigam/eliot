module Processor.TestCompiler (compileSourceCode, compileSelectFact, compileCollectFacts) where

import Engine.FactEngine
import CompilerProcessor
import Data.Maybe

compileSourceCode :: [CompilerProcessor] -> String -> IO [(Signal, Fact)]
compileSourceCode processors code = (fromMaybe []) <$> (resolveFacts processors [(SourceFileRead "", SourceFileContent "" code)])

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

