{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Processor.ASTSpec (spec) where

import Test.Hspec
import Processor.Token
import Processor.Error
import Processor.AST
import Processor.TestCompiler
import CompilerProcessor

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
         parseForErrors "import a.b.c" `shouldReturn` ["Parser error, unexpected end of input, expecting symbol '.'."]

      it "should not parse import on multiple lines even if correct" $ do
         parseForErrors "import a.\nb.c" `shouldReturn` ["Parser error, unexpected identifier \"b\", expecting end of input or top level keyword \"import\"."]

      it "should report multiple errors" $ do
         parseForErrors "import a.b.c\nimport d;e\n" >>= (`shouldMatchList` ["Parser error, unexpected keyword \"import\", expecting symbol '.'.", "Parser error, unexpected symbol \";\", expecting symbol '.'."])

      it "should force import statement to begin on first column" $ do
         parseForErrors " import a.b.C" `shouldReturn` ["Parser error, unexpected keyword \"import\", expecting function name."]

      it "should reject keyword 'import' as function name" $ do
         parseForErrors "a() = b\nimport = a\n" `shouldReturn` ["Parser error, unexpected keyword \"import\", expecting function name."]

      it "should parse function declaration with parens without parameters" $ do
         parseForErrors "a() = b" `shouldReturn` []

      it "should parse function declaration of a number literal" $ do
         parseForErrors "a = 123" `shouldReturn` []

      it "should parse function declaration with parens and a single parameter" $ do
         parseForErrors "a(n) = b" `shouldReturn` []

      it "should parse function declaration with parens and a two parameters" $ do
         parseForErrors "a(n, m) = b" `shouldReturn` []

      it "should not parse function declaration with parens and missing comma" $ do
         parseForErrors "a(n m) = b" `shouldReturn` ["Parser error, unexpected identifier \"m\", expecting symbol ',' or symbol ')'."]

      it "should parse function declaration without parens" $ do
         parseForErrors "a = b" `shouldReturn` []

      it "should parse function call with parens" $ do
         parseForErrors "a = b()" `shouldReturn` []

      it "should parse function call with number literal" $ do
         parseForErrors "a = b(1)" `shouldReturn` []

      it "should parse function call with another function call" $ do
         parseForErrors "a = b(c())" `shouldReturn` []

      it "should parse function call with two parameters" $ do
         parseForErrors "a = b(123, c())" `shouldReturn` []

parseForErrors :: String -> IO [String]
parseForErrors code = compileCollectFacts [simpleProcessor parseTokensProcessor, simpleProcessor parseASTProcessor] [("", code)] selectErrors
   where selectErrors :: (CompilerError, CompilerError) -> Maybe String
         selectErrors (_, CompilerError _ _ _ msg) = Just msg

parseForAST :: String -> IO AST
parseForAST code = compileSelectFact [simpleProcessor parseTokensProcessor, simpleProcessor parseASTProcessor] [("", code)] selectAST
   where selectAST :: (SourceASTSignal, SourceAST) -> Maybe AST
         selectAST (_, SourceAST _ ast) = Just ast

extractImports (AST imps _) = map toImportTest imps

toImportTest (Import _ ps (PositionedToken _ _ _ (Identifier m))) = ImportTest (map toImportTestPackage ps) m
toImportTest _                                                  = ImportTest [] ""

toImportTestPackage (PositionedToken _ _ _ (Identifier p)) = p
toImportTestPackage _                                    = ""

data ImportTest = ImportTest [String] String
   deriving (Eq, Show)

