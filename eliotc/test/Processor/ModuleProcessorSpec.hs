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

      it "should decline lower case module names" $ do
         parseForErrors "testFile" "" `shouldReturn` ["Module name must be capitalized."]

      it "should indicate error when imported module is not found" $ do
         parseForErrors "TestFile" "import A" `shouldReturn` ["Could not find imported module."]

      it "should indicate error when function name is capitalized" $ do
         parseForErrors "TestFile" "One = a" `shouldReturn` ["Functions must begin with a lowercase letter or be an operator."]

      it "should detect the same function name declared twice" $ do
         parseForErrors "TestFile" "one = a\none = b" `shouldReturn` ["Function already declared."]
         
      it "should detect the same function imported twice" $ do
         parseMultiForErrors [("TestFile","import A\nimport B"), ("A", "one = a"), ("B", "one = b")] `shouldReturn` ["Imported module imports functions that are already in scope: [\"one\"]."]
         
      it "should detect the defined and imported function name collision" $ do
         parseMultiForErrors [("TestFile","import A\none = a"), ("A", "one = a")] `shouldReturn` ["Imported module imports functions that are already in scope: [\"one\"]."]

parseForErrors :: String -> String -> IO [String]
parseForErrors filename code = parseMultiForErrors [(filename, code)]

parseMultiForErrors :: [(String, String)] -> IO [String]
parseMultiForErrors files = compileCollectFacts [parseTokensProcessor, parseASTProcessor, parseModuleProcessor] files selectErrors
   where selectErrors (_, CompilerErrorFact (CompilerError _ _ _ msg)) = Just msg
         selectErrors _                          = Nothing

parseForFact :: String -> String -> IO (ModuleName, [String])
parseForFact filename code = parseMultiForFact [(filename, code)]

parseMultiForFact :: [(String, String)] -> IO (ModuleName, [String])
parseMultiForFact files = compileSelectFact [parseTokensProcessor, parseASTProcessor, parseModuleProcessor] files selectFact
   where selectFact (_, ModuleFunctionNames mn names) = Just (mn, names)
         selectFact _                                 = Nothing

