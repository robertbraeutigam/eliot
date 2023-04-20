{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (Token(..)) where

data Token = Identifier String   -- Satisfies the rules for a generic identifier ~alphanumeric
           | Symbol String       -- Sort-of identifier comprised of non-alphanumberic characters
   deriving (Show)

