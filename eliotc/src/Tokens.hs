{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Tokens (PositionedToken(..), Token(..), tokenLength) where

import Text.Parsec

data PositionedToken = PositionedToken FilePath Line Column Token
   deriving (Eq)

data Token = Identifier String   -- Satisfies the rules for a generic identifier ~alphanumeric
           | Symbol String       -- Sort-of identifier comprised of non-alphanumberic characters
   deriving (Eq)

instance Show PositionedToken where
   show (PositionedToken _ _ _ t) = show t

instance Show Token where
   show (Identifier i) = "identifier \""++i++"\""
   show (Symbol i) = "symbol \""++i++"\""

tokenLength :: Token -> Int
tokenLength (Identifier str) = length str
tokenLength (Symbol str)     = length str

