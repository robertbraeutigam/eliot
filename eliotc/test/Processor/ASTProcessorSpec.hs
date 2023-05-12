{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.ASTProcessorSpec (spec) where

import Test.Hspec
import Data.Either.Extra
import Tokens
import Processor.TokensProcessor
import AST
import Processor.ASTProcessor

spec :: Spec
spec = do
   describe "parsing ast" $ do
      it "should successfully parse an empty source file" $ do
         extractImports (parseCode "") `shouldBe` []

      it "should parse a correct import statement" $ do
         extractImports (parseCode "import a.b.C") `shouldBe` [ImportTest ["a", "b"] "C"]

      it "should parse multiple correct import statements" $ do
         extractImports (parseCode "import a.b.C\nimport b.c.D\n\nimport e.f.h.G") `shouldBe` [ImportTest ["a", "b"] "C", ImportTest ["b", "c"] "D", ImportTest ["e", "f", "h"] "G"]

      it "should not parse import with non-capitalized module name" $ do
         countASTErrors (parseCode "import a.b.c") `shouldBe` 1

      it "should report multiple errors" $ do
         countASTErrors (parseCode "import a.b.c\nimport d;e\n") `shouldSatisfy` (>=2)

      it "should force import statement to begin on first column" $ do
         countASTErrors (parseCode " import a.b.C") `shouldBe` 1

countASTErrors (Left _)          = 0
countASTErrors (Right (errs, _)) = length errs

parseCode code = mapRight parseAST (parseTokens code)

extractImports (Left _)             = []
extractImports (Right (_, AST imps _)) = map toImportTest imps

toImportTest (Import _ ps (PositionedToken _ _ (Identifier m))) = ImportTest (map toImportTestPackage ps) m
toImportTest _                                                  = ImportTest [] ""

toImportTestPackage (PositionedToken _ _ (Identifier p)) = p
toImportTestPackage _                                    = ""

data ImportTest = ImportTest [String] String
   deriving (Eq, Show)

