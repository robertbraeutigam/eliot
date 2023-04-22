module TokensSpec (spec) where

import Data.Either
import Test.Hspec
import Tokens

spec :: Spec
spec = do
   describe "parsing tokens" $ do
      it "should not return tokens in line comments" $ do
         parseTokens "// not tokens" `shouldBe` Right []

      it "should return tokens before line comments" $ do
         parseTokens "token // not tokens" `shouldBe` Right [Identifier "token"]

      it "should return tokens after line comments in the next line" $ do
         parseTokens "// not tokens\nagain" `shouldBe` Right [Identifier "again"]

      it "should not return tokens in block comments" $ do
         parseTokens "/* not tokens */" `shouldBe` Right []

      it "should not return tokens in block comments with multiple lines" $ do
         parseTokens "/* not tokens\nagain no\ntokens here */" `shouldBe` Right []

      it "should return tokens before block comments" $ do
         parseTokens "token /* not tokens */" `shouldBe` Right [Identifier "token"]

      it "should return tokens after block comments" $ do
         parseTokens "/* not tokens */again" `shouldBe` Right [Identifier "again"]

      it "should fail on broken block comment" $ do
         isLeft (parseTokens "/* block comment not balanced") `shouldBe` True

      it "should not parse illegal tokens" $ do
         isLeft (parseTokens "→") `shouldBe` True

