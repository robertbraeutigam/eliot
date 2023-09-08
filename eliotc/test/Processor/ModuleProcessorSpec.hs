{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.ModuleProcessorSpec (spec) where

import Test.Hspec
import Processor.TokensProcessor
import Processor.ASTProcessor
import Processor.ModuleProcessor
import Processor.TestCompiler
import CompilerProcessor
import Module

spec :: Spec
spec = do
   describe "parsing the module" $ do
      it "should determine module name from file name" $ do
         fst <$> (parseForFact "TestFile" "") `shouldReturn` (ModuleName [] "TestFile")

      it "should not create the module if the module name is lower case" $ do
         parseForFact "testFile" "" `shouldThrow` anyErrorCall

      it "should detect and return all the function names" $ do
         snd <$> (parseForFact "TestFile" "one = a\ntwo = b") `shouldReturn` ["one", "two"]

parseForFact :: String -> String -> IO (ModuleName, [String])
parseForFact filename code = compileSelectFact [parseTokensProcessor, parseASTProcessor, parseModuleProcessor] filename code selectFact
   where selectFact (_, ModuleFunctionNames mn names) = Just (mn, names)
         selectFact _                                 = Nothing

