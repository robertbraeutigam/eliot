module FactEngineSpec (spec) where

import Test.Hspec
import FactEngine
import Control.Monad

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

      it "should return nothing if processor depends on non-existing fact" $ do
         resolveFacts [dependsOnX] [("x", "a")] `shouldReturn` Nothing

      it "should execute chains of fact processors" $ do
         result <- resolveFacts [aToB, bToC] [("a", "a")]
         case result of
            Just xs -> xs `shouldMatchList` [("a", "a"), ("y", "b"), ("x", "c")]
            Nothing -> expectationFailure "engine returned nothing unexpectedly"

      it "should execute test that is waiting for an additional fact" $ do
         result <- resolveFacts [aToB, bToC, abcToD] [("a", "a")]
         case result of
            Just xs -> xs `shouldMatchList` [("a", "a"), ("y", "b"), ("x", "c"), ("z", "c")]
            Nothing -> expectationFailure "engine returned nothing unexpectedly"

aToB :: FactProcessor String String
aToB "a" = registerFact "y" "b"
aToB _ = return ()

bToC :: FactProcessor String String
bToC "b" = registerFact "x" "c"
bToC _ = return ()

abcToD :: FactProcessor String String
abcToD "a" = getFact "x" >> getFact "y" >> (registerFact "z" "c")
abcToD _ = return ()

broken :: FactProcessor String String
broken _ = error "broken processor failure"

dependsOnX :: FactProcessor String String
dependsOnX _ = void $ getFact "X"

