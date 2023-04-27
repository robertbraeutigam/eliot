module TokensSpec (spec) where

import Data.Either
import Test.Hspec
import Data.Either.Extra
import Tokens
import CompilerError

spec :: Spec
spec = do
   describe "parsing tokens" $ do
      it "should not return tokens in line comments" $ do
         parseJustTokens "// not tokens" `shouldBe` Right []

      it "should return tokens before line comments" $ do
         parseJustTokens "token // not tokens" `shouldBe` Right [Identifier "token"]

      it "should return tokens after line comments in the next line" $ do
         parseJustTokens "// not tokens\nagain" `shouldBe` Right [Identifier "again"]

      it "should not return tokens in block comments" $ do
         parseJustTokens "/* not tokens */" `shouldBe` Right []

      it "should return the correct position of a token" $ do
         parseTokens " some\ntokens" `shouldBe` Right [(PositionedToken 1 2 (Identifier "some")), (PositionedToken 2 1 (Identifier "tokens"))]

      it "should include comments in the position of a token" $ do
         parseTokens " /* Some Comment */ Token" `shouldBe` Right [(PositionedToken 1 21 (Identifier "Token"))]

      it "should not return tokens in block comments with multiple lines" $ do
         parseJustTokens "/* not tokens\nagain no\ntokens here */" `shouldBe` Right []

      it "should return tokens before block comments" $ do
         parseJustTokens "token /* not tokens */" `shouldBe` Right [Identifier "token"]

      it "should return tokens after block comments" $ do
         parseJustTokens "/* not tokens */again" `shouldBe` Right [Identifier "again"]

      it "should fail on broken block comment" $ do
         isLeft (parseJustTokens "/* block comment not balanced") `shouldBe` True

      it "should not parse illegal tokens" $ do
         isLeft (parseJustTokens "→") `shouldBe` True

parseJustTokens :: String -> Either CompilerError [Token]
parseJustTokens str = mapRight (map positionedToken) (parseTokens str)

