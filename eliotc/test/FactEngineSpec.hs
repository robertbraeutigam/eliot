module FactEngineSpec (spec) where

import Test.Hspec
import FactEngine

spec :: Spec
spec = do
   describe "resolve facts" $ do
      it "should terminate with no facts if there are no initial facts" $ do
         resolveFacts [] [] `shouldReturn` Just ([] :: [(String, String)])

      it "should terminate with initial facts if no processors given" $ do
         result <- resolveFacts [] [("a", "b"), ("c", "d")]
         case result of
            Just xs -> xs `shouldMatchList` [("a", "b"), ("c", "d")]
            Nothing -> expectationFailure "engine returned nothing unexpectedly"

      it "should start processors on the initial facts" $ do
         result <- resolveFacts [aToB] [("x", "a")]
         case result of
            Just xs -> xs `shouldMatchList` [("x", "a"), ("y", "b")]
            Nothing -> expectationFailure "engine returned nothing unexpectedly"

      it "should fail if a processor fails" $ do
         resolveFacts [broken] [("x", "a")] `shouldThrow` anyException

      it "should fail if multiple processors fails" $ do
         resolveFacts [broken, broken, broken] [("x", "a")] `shouldThrow` anyException

aToB :: FactProcessor String String
aToB engine "a" = registerFact engine "y" "b"
aToB _ _ = return ()

broken :: FactProcessor String String
broken _ _ = error "broken processor failure"
