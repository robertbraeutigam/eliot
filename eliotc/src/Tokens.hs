{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (PositionedToken(..), Token(..), parseTokens) where

import Text.Parsec
import Data.Char (isSpace)
import Control.Monad

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

parseTokens :: String -> Either ParseError [PositionedToken]
parseTokens code = parse (whiteSpace >> (many anyTokenLexeme) <* eof) "" code

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

