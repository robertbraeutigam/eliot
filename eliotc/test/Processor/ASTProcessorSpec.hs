{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.ASTProcessorSpec (spec) where

import Test.Hspec
import Tokens
import Processor.TokensProcessor
import AST
import Processor.ASTProcessor
import CompilerProcessor
import Processor.TestCompiler

spec :: Spec
spec = do
   describe "parsing ast" $ do
      it "should successfully parse an empty source file" $ do
         extractImports <$> (parseForAST "") `shouldReturn` []

      it "should parse a correct import statement" $ do
         extractImports <$> (parseForAST "import a.b.C") `shouldReturn` [ImportTest ["a", "b"] "C"]

      it "should parse multiple correct import statements" $ do
         extractImports <$> (parseForAST "import a.b.C\nimport b.c.D\n\nimport e.f.h.G") `shouldReturn` [ImportTest ["a", "b"] "C", ImportTest ["b", "c"] "D", ImportTest ["e", "f", "h"] "G"]

      it "should not parse import with non-capitalized module name" $ do
         length <$> (parseForErrors "import a.b.c") `shouldReturn` 1

      it "should not parse import on multiple lines even if correct" $ do
         length <$> (parseForErrors "import a.\nb.c") `shouldReturn` 1

      it "should report multiple errors" $ do
         length <$> (parseForErrors "import a.b.c\nimport d;e\n") `shouldReturn` 2

      it "should force import statement to begin on first column" $ do
         length <$> (parseForErrors " import a.b.C") `shouldReturn` 1

parseForErrors :: String -> IO [CompilerError]
parseForErrors code = compileCollectFacts [parseTokensProcessor, parseASTProcessor] [("", code)] selectErrors
   where selectErrors (_, CompilerErrorFact err) = Just err
         selectErrors _                          = Nothing

parseForAST :: String -> IO AST
parseForAST code = compileSelectFact [parseTokensProcessor, parseASTProcessor] [("", code)] selectAST
   where selectAST (_, SourceAST _ ast) = Just ast
         selectAST _                    = Nothing

extractImports (AST imps _) = map toImportTest imps

toImportTest (Import _ ps (PositionedToken _ _ _ (Identifier m))) = ImportTest (map toImportTestPackage ps) m
toImportTest _                                                  = ImportTest [] ""

toImportTestPackage (PositionedToken _ _ _ (Identifier p)) = p
toImportTestPackage _                                    = ""

data ImportTest = ImportTest [String] String
   deriving (Eq, Show)

