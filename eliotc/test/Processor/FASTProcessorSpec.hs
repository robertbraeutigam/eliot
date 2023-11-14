{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.FASTProcessorSpec (spec) where

import Test.Hspec
import Processor.TokensProcessor
import Processor.ASTProcessor
import Processor.ModuleProcessor
import Processor.FASTProcessor
import Processor.TestCompiler
import CompilerProcessor
import FAST
import Module

spec :: Spec
spec = do
   describe "parsing fast" $ do
      it "should result in a function application when parsing a function call" $ do
         parseForFunction "ni = nu\nnu = a" "ni" `shouldReturn` (FunctionApplication $ FunctionFQN (ModuleName [] "A") "nu")

      it "should indicate error when referred function is not defined" $ do
         parseForErrors "ni = nu" `shouldReturn` ["Called function not defined."]
         
      it "should compile native function" $ do
         parseForErrors "ni = native" `shouldReturn` []
         
parseForErrors :: String -> IO [String]
parseForErrors code = compileCollectFacts [parseTokensProcessor, parseASTProcessor, parseModuleProcessor, parseFASTProcessor] [("A", code)] selectErrors
   where selectErrors (_, CompilerErrorFact (CompilerError _ _ _ msg)) = Just msg
         selectErrors _                                                = Nothing


parseForFunction :: String -> String -> IO FunctionBody
parseForFunction code func = compileSelectFact [parseTokensProcessor, parseASTProcessor, parseModuleProcessor, parseFASTProcessor] [("A", code)] selectFact
   where selectFact (_, CompiledFunction (FunctionFQN _ f) body) | f == func = Just body
         selectFact _                                                        = Nothing

