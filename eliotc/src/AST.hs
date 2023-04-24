{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module AST (AST, parseAST) where

import Tokens
import Text.Parsec
import Data.Char (isLower, isUpper)

data Import = Import [String] String  -- Import statement with pacakges and module name
   deriving (Show, Eq)

data AST = AST { 
   importStatements :: [Import]
}
   deriving (Show, Eq)

parseAST :: [Token] -> Either ParseError AST
parseAST = parse (parseSource <* eof) ""

parseSource = do
   imps <- many importStatement
   return $ AST imps

importStatement = do
   _    <- keyword "import"
   pkgs <- many (packageName <* (symbol "."))
   mod  <- moduleName
   return $ Import pkgs mod

-- Low level stuff

moduleName = satisfyT (\t -> case t of
   Identifier id@(c:cs) -> if isUpper c then Just id else Nothing
   _                   -> Nothing) <?> "module name"

packageName = satisfyT (\t -> case t of
   Identifier id@(c:cs) -> if isLower c then Just id else Nothing
   _                   -> Nothing) <?> "package name"

keyword name = satisfyT (\t -> case t of
   Identifier id -> if id == name then Just id else Nothing
   _             -> Nothing) <?> ("keyword "++(show name))

symbol name = satisfyT (\t -> case t of
   Symbol id -> Just id
   _         -> Nothing) <?> "symbol"

-- | Using the primitive token function to maybe parse a token and produce
-- an output. We use the output to unpack Tokens.
satisfyT :: (Token -> Maybe a) -> Parsec [Token] () a
satisfyT f = tokenPrim show nextPos f
   where nextPos currentPos t stream = currentPos -- TODO
