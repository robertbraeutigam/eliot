{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (Token(..), parseTokens) where

import Text.Parsec
import Data.Char (isSpace)
import Control.Monad

data Token = Identifier String   -- Satisfies the rules for a generic identifier ~alphanumeric
           | Symbol String       -- Sort-of identifier comprised of non-alphanumberic characters
   deriving (Show, Eq)

parseTokens :: String -> Either ParseError [Token]
parseTokens code = parse (whiteSpace >> (many ((identifier <|> operator) <* whiteSpace)) <* eof) "" code

identifier = do
   firstCharacter <- letter
   restCharacters <- many (alphaNum <|> oneOf "_'")
   return $ Identifier (firstCharacter:restCharacters)

operator = (many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~") >>= (return . Symbol)

-- | Whitespace includes everything from spaces, newlines to comments.
whiteSpace = skipMany $ simpleSpace <|> oneLineComment <|> multiLineComment

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = try (string "//") >> skipMany (satisfy (/= '\n'))

multiLineComment = try (string "/*") >> multiLineCommentBody

multiLineCommentBody = (void $ try (string "*/"))                             -- Comment ended
                   <|> (skipMany1 (satisfy (/= '*')) >> multiLineCommentBody) -- Skip to next *
                   <|> ((void $ char '*') >> multiLineCommentBody)            -- Skip * if string "*/" didn't match

