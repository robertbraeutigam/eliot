{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ASTSpec (spec) where

import Test.Hspec
import Data.Either.Extra
import Tokens
import AST

spec :: Spec
spec = do
   describe "parsing ast" $ do
      it "should parse a correct import statement" $ do
         parseCode "import a.b.C" `shouldBe` importAST [Import ["a", "b"] "C"]

      it "should parse multiple correct import statements" $ do
         parseCode "import a.b.C\nimport b.c.D\n\nimport e.f.h.G" `shouldBe` importAST [Import ["a", "b"] "C", Import ["b", "c"] "D", Import ["e", "f", "h"] "G"]

      it "should not parse import with non-capitalized module name" $ do
         countASTErrors (parseCode "import a.b.c") `shouldBe` 1

      it "should report multiple errors" $ do
         countASTErrors (parseCode "import a.b.c\nimport d;e\n") `shouldSatisfy` (>=2)

      it "should force import statement to begin on first column" $ do
         countASTErrors (parseCode " import a.b.C") `shouldBe` 1

countASTErrors result = case result of
   Left _            -> 0
   Right (errs, _)   -> length errs

importAST imps = Right ([], AST imps)

parseCode code = mapRight parseAST (parseTokens code)

