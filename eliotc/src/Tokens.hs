{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (PositionedToken(..), Token(..), tokenContent, tokenLength, positionedTokenContent) where

import Text.Parsec

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
