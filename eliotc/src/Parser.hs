{-| Parser compiler processor that creates and AST from source code.
 -}

module Parser (parserProcessor) where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Lang
import AST
import CompilerFacts
import Logging

parserProcessor :: CompilerProcessor
parserProcessor (SourceFileContent code) = debugMsg $ show $ parse (wholeFile moduleParser) "filename" code
parserProcessor _ = return ()

moduleParser = many importParser

importParser = reserved "import"

languageDef = emptyDef
   { Lang.commentStart   = "/*"
   , Lang.commentEnd     = "*/"
   , Lang.commentLine    = "//"
   , Lang.nestedComments = True
   , Lang.identStart     = letter
   , Lang.identLetter    = alphaNum <|> oneOf "_'"
   , Lang.reservedNames  = ["where", "import"]
   , Lang.reservedOpNames= ["=", ":", "->", "<-", "."]
   , Lang.caseSensitive  = True
   }

lexer = Lang.makeTokenParser languageDef

whiteSpace = Lang.whiteSpace lexer

reserved = Lang.reserved lexer

wholeFile p = whiteSpace >> p <* eof

