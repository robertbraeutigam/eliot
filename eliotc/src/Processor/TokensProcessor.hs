{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Processor.TokensProcessor (parseTokensProcessor) where

import Text.Parsec
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Control.Monad
import CompilerProcessor
import Processor.FileProcessors
import Tokens
import Processor.Error

-- | Parse tokens if a source file is read.
parseTokensProcessor :: CompilerProcessor
parseTokensProcessor v = case getTypedValue v of
   Just (SourceFileContent path code) -> case parse (whiteSpace >> (many anyTokenLexeme) <* eof) path code of
      Left parserError -> compilerErrorTranslated path parserError
      Right ts         -> registerCompilerFact (SourceTokensSignal path) (SourceTokens path ts)
   _                                  -> compileOk

anyTokenLexeme = ((identifierOrKeyword <|> symbol <|> singleSymbol <|> number) <* whiteSpace) <?> "legal character"

identifierOrKeyword = do
   firstCharacter <- letter
   restCharacters <- many (alphaNum <|> oneOf "_'")
   returnPositionedToken (toToken (firstCharacter:restCharacters))
   where toToken content = if content `elem` keywords then Keyword content else Identifier content

symbol = (do
   sym <- (many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~;")
   returnPositionedToken (Symbol sym)) <?> "operator"

singleSymbol = (do 
   sym <- oneOf "(),"
   returnPositionedToken $ Symbol [sym]) <?> "single operator character"

number = (do
   digits <- (many1 $ oneOf "0123456789")
   returnPositionedToken (NumberLiteral digits)) <?> "number"

returnPositionedToken t = do
   pos <- getPosition
   return $ PositionedToken (sourceName pos) (sourceLine pos) ((sourceColumn pos)-(length $ tokenContent t)) t

-- | Whitespace includes everything from spaces, newlines to comments.
whiteSpace = skipMany $ simpleSpace <|> ((oneLineComment <|> multiLineComment) <?> "comment")

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = try (string "//") >> skipMany (satisfy (/= '\n'))

multiLineComment = try (string "/*") >> multiLineCommentBody

multiLineCommentBody = ((void $ try (string "*/"))                             -- Comment ended
                   <|> (skipMany1 (satisfy (/= '*')) >> multiLineCommentBody) -- Skip to next *
                   <|> ((void $ char '*') >> multiLineCommentBody))            -- Skip * if string "*/" didn't match
                   <?> "closing '*/' of block comment"

keywords = ["import", "native"]

-- Translate errors

compilerErrorTranslated fp e = compilerError fp pos pos (translateParsecErrorMessage $ show e)
   where pos = SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

