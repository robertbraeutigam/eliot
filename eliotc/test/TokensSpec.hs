module TokensSpec (spec) where

import Test.Hspec
import Tokens

spec :: Spec
spec = do
   describe "parsing tokens" $ do
      it "should not return tokens in line comments" $ do
         parseTokens "token // not tokens" `shouldBe` Right [Identifier "token"]


