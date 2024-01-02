{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Processor.Token (compilerErrorForTokens, parseTokensProcessor, PositionedToken(..), Token(..), tokenContent, positionedTokenContent, tokenLength, SourceTokensSignal(..), SourceTokens(..)) where

import Data.Hashable
import GHC.Generics
import Text.Parsec
import Processor.Error
import Processor.Source
import CompilerProcessor
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Control.Monad

data PositionedToken = PositionedToken FilePath Line Column Token
   deriving (Eq)

data Token = Identifier String     -- Satisfies the rules for a generic identifier ~alphanumeric
           | Symbol String         -- Sort-of identifier comprised of non-alphanumberic characters
           | Keyword String        -- Keywords that are reserved to be used in certain places but no others
           | NumberLiteral String  -- A number literal
   deriving (Eq)

instance Show PositionedToken where
   show (PositionedToken _ _ _ t) = show t

instance Show Token where
   show (Identifier i) = "identifier \""++i++"\""
   show (Symbol i) = "symbol \""++i++"\""
   show (Keyword i) = "keyword \""++i++"\""
   show (NumberLiteral i) = "number \""++i++"\""

tokenContent :: Token -> String
tokenContent (Identifier str)     = str
tokenContent (Symbol str)         = str
tokenContent (Keyword str)        = str
tokenContent (NumberLiteral str)  = str

tokenLength :: Token -> Int
tokenLength = length . tokenContent

positionedTokenContent (PositionedToken _ _ _ t) = tokenContent t

data SourceTokensSignal = SourceTokensSignal FilePath
   deriving (Eq, Generic)

instance Hashable SourceTokensSignal

-- | Tokens read from a source file
data SourceTokens = SourceTokens FilePath [PositionedToken]

compilerErrorForTokens :: [PositionedToken] -> String -> CompilerIO ()
compilerErrorForTokens [] _ = error "Compiler error was invoked on no tokens."
compilerErrorForTokens pts@((PositionedToken file _ _ _):_) msg = compilerError file fromFirstToken toLastToken msg
   where fromFirstToken = pos $ head pts
         toLastToken    = case pos $ last pts of
            (SourcePosition line column) -> SourcePosition line (column + (length $ positionedTokenContent (last pts)))
         pos (PositionedToken _ line column _) = SourcePosition line column

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

