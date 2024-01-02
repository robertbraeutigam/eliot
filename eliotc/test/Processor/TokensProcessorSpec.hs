module Processor.TokensProcessorSpec (spec) where

import Test.Hspec
import Tokens
import Processor.TokensProcessor
import Processor.TestCompiler
import Processor.Error
import CompilerProcessor

spec :: Spec
spec = do
   describe "parsing tokens" $ do
      it "should not return tokens in line comments" $ do
         parseForTokens "// not tokens" `shouldReturn` []

      it "should return tokens before line comments" $ do
         parseForTokens "token // not tokens" `shouldReturn` [Identifier "token"]

      it "should return tokens after line comments in the next line" $ do
         parseForTokens "// not tokens\nagain" `shouldReturn` [Identifier "again"]

      it "keywords are parsed into keyword" $ do
         parseForTokens "import notkeyword" `shouldReturn` [Keyword "import", Identifier "notkeyword"]

      it "should not return tokens in block comments" $ do
         parseForTokens "/* not tokens */" `shouldReturn` []

      it "should return the correct position of a token" $ do
         parseForPositionedTokens " some\ntokens" `shouldReturn` [(PositionedToken "" 1 2 (Identifier "some")), (PositionedToken "" 2 1 (Identifier "tokens"))]

      it "should include comments in the position of a token" $ do
         parseForPositionedTokens " /* Some Comment */ Token" `shouldReturn` [(PositionedToken "" 1 21 (Identifier "Token"))]

      it "should not return tokens in block comments with multiple lines" $ do
         parseForTokens "/* not tokens\nagain no\ntokens here */" `shouldReturn` []

      it "should return tokens before block comments" $ do
         parseForTokens "token /* not tokens */" `shouldReturn` [Identifier "token"]

      it "should return tokens after block comments" $ do
         parseForTokens "/* not tokens */again" `shouldReturn` [Identifier "again"]

      it "should fail on broken block comment" $ do
         length <$> (parseForErrors "/* block comment not balanced") `shouldReturn` 1

      it "should not parse illegal tokens" $ do
         length <$> (parseForErrors "→") `shouldReturn` 1

parseForPositionedTokens :: String -> IO [PositionedToken]
parseForPositionedTokens code = compileSelectFact [parseTokensProcessor] [("", code)] selectTokens
   where selectTokens :: (Signal, Fact) -> Maybe [PositionedToken]
         selectTokens (_, SourceTokens _ pts) = Just pts

parseForTokens :: String -> IO [Token]
parseForTokens source = (map (\(PositionedToken _ _ _ t) -> t)) <$> parseForPositionedTokens source

parseForErrors :: String -> IO [String]
parseForErrors code = compileCollectFacts [parseTokensProcessor] [("", code)] selectErrors
   where selectErrors :: (CompilerError, CompilerError) -> Maybe String
         selectErrors (_, CompilerError _ _ _ msg) = Just msg

