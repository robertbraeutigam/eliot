{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (PositionedToken(..), Token(..), parseTokens, tokenLength) where

import Text.Parsec
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Control.Monad
import CompilerError

data PositionedToken = PositionedToken {
      positionedTokenLine::Line,
      positionedTokenColumn::Column,
      positionedToken::Token
   }
   deriving (Eq)

instance Show PositionedToken where
   show (PositionedToken _ _ t) = show t

data Token = Identifier String   -- Satisfies the rules for a generic identifier ~alphanumeric
           | Symbol String       -- Sort-of identifier comprised of non-alphanumberic characters
   deriving (Show, Eq)

tokenLength :: Token -> Int
tokenLength (Identifier str) = length str
tokenLength (Symbol str)     = length str

parseTokens :: String -> Either CompilerError [PositionedToken]
parseTokens code = case parse (whiteSpace >> (many anyTokenLexeme) <* eof) "" code of
   Left e -> Left $ translateTokenizerError e
   Right ts -> Right ts

anyTokenLexeme = ((identifier <|> symbol) <* whiteSpace) <?> "legal character"

identifier = do
   firstCharacter <- letter
   restCharacters <- many (alphaNum <|> oneOf "_'")
   pos            <- getPosition
   return $ PositionedToken (sourceLine pos) ((sourceColumn pos)-1-(length restCharacters)) (Identifier (firstCharacter:restCharacters))

symbol = (do
   sym <- (many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~;")
   pos <- getPosition
   return $ PositionedToken (sourceLine pos) ((sourceColumn pos)-(length sym)) (Symbol sym)) <?> "operator"

-- | Whitespace includes everything from spaces, newlines to comments.
whiteSpace = skipMany $ simpleSpace <|> ((oneLineComment <|> multiLineComment) <?> "comment")

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = try (string "//") >> skipMany (satisfy (/= '\n'))

multiLineComment = try (string "/*") >> multiLineCommentBody

multiLineCommentBody = ((void $ try (string "*/"))                             -- Comment ended
                   <|> (skipMany1 (satisfy (/= '*')) >> multiLineCommentBody) -- Skip to next *
                   <|> ((void $ char '*') >> multiLineCommentBody))            -- Skip * if string "*/" didn't match
                   <?> "closing '*/' of block comment"

-- Translate errors

translateTokenizerError e = CompilerError pos pos (translateParsecErrorMessage $ show e)
   where pos = SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

