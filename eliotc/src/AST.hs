{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module AST (AST(..), Import(..), FunctionDefinition(..)) where

import Tokens

data Import = Import {
   importKeyword::PositionedToken,
   importPackageNames::[PositionedToken],
   importModule::PositionedToken
}  
   deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition {
   functionSignature::[PositionedToken],
   functionBody::[PositionedToken]
}
   deriving (Show, Eq)

data AST = AST { 
   importStatements    :: [Import],
   functionDefinitions :: [FunctionDefinition]
}
   deriving (Show, Eq)

