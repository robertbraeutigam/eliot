{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module AST (AST(..), Import(..), FunctionDefinition(..), FunctionBodyTokens(..), allImportTokens, ExpressionTokens(..)) where

import Tokens

data Import = Import {
   importKeyword::PositionedToken,
   importPackageNames::[PositionedToken],
   importModule::PositionedToken
}  
   deriving (Show, Eq)

allImportTokens :: Import -> [PositionedToken]
allImportTokens (Import k pns m) = [k] ++ pns ++ [m]

data FunctionDefinition = FunctionDefinition {
   functionName::PositionedToken,
   functionParameters::[PositionedToken],
   functionBody::FunctionBodyTokens
}
   deriving (Show, Eq)

data FunctionBodyTokens = NativeFunctionToken | Expression ExpressionTokens
 deriving (Eq, Show)

data ExpressionTokens = FunctionApplicationTokens PositionedToken [ExpressionTokens]
                      | NumberLiteralToken PositionedToken
 deriving (Eq, Show)

data AST = AST { 
   importStatements    :: [Import],
   functionDefinitions :: [FunctionDefinition]
}
   deriving (Show, Eq)

