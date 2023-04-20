{-| AST of ELIOT
 -}

module AST (ImportStatement(..)) where

data ImportStatement = Import [String] String

