{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.FASTProcessorSpec (spec) where

import Test.Hspec
import Processor.TokensProcessor
import Processor.ASTProcessor
import Processor.FASTProcessor
import Processor.TestCompiler
import CompilerProcessor
import FAST

spec :: Spec
spec = do
   describe "parsing fast" $ do
      it "should determine module name from file name" $ do
         extractModuleName <$> (parseForFAST "TestFile" "") `shouldReturn` (ModuleName [] "TestFile")

      it "should not create the module if the module name is lower case" $ do
         parseForFAST "testFile" "" `shouldThrow` anyErrorCall
         

parseForErrors :: String -> IO [CompilerError]
parseForErrors code = compileCollectFacts [parseTokensProcessor, parseASTProcessor, parseFASTProcessor] "TestFile" code selectErrors
   where selectErrors (_, CompilerErrorFact err) = Just err
         selectErrors _                          = Nothing

parseForFAST :: String -> String -> IO Module
parseForFAST filename code = compileSelectFact [parseTokensProcessor, parseASTProcessor, parseFASTProcessor] filename code selectFAST
   where selectFAST (_, ModuleFAST fast) = Just fast
         selectFAST _                      = Nothing

extractModuleName (Module name _) = name
