{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module AST (AST, parseAST) where

import Tokens
import Text.Parsec
import Text.Parsec.Pos
import Data.Maybe
import Data.Char (isLower, isUpper)

data Import = Import [String] String  -- Import statement with pacakges and module name
   deriving (Show, Eq)

data AST = AST { 
   importStatements :: [Import]
}
   deriving (Show, Eq)

-- | Run the parser with all features
parseAST :: [PositionedToken] -> ([ParseError], AST)
parseAST [] = ([], AST [])
parseAST (t:ts) = either (\e -> ([e], AST [])) id $
   runParser (positionedRecoveringParseSource t) [] "" (t:ts)

-- | Set the source position of the first token explicitly before the parsing starts.
positionedRecoveringParseSource t =
   (setPosition $ newPos "" (positionedTokenLine t) (positionedTokenColumn t)) >> recoveringParseSource

-- | Recover from all errors of the parser and store the error in the user state instead.
recoveringParseSource = do
   ast    <- parseSource
   errors <- getState
   return (reverse errors, ast)

parseSource = do
   imps <- many $ importStatement `recoverWith` skipLine
   _    <- eof
   return $ AST (catMaybes imps)

importStatement = do
   _    <- keyword "import"
   pkgs <- many (packageName <* (symbol "."))
   mod  <- moduleName
   return $ Import pkgs mod

-- Low level stuff
skipLine = skipOne >> (skipMany $ satisfyPT (\pt -> if (positionedTokenColumn pt) /= 1 then Just () else Nothing))

skipOne = satisfyT (const Just ())

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
   Symbol id -> if id == name then Just id else Nothing
   _         -> Nothing) <?> ("symbol '" ++ name ++ "'")


type ASTParser = Parsec [PositionedToken] [ParseError]

-- | Using the primitive token function to maybe parse a token and produce
-- an output. We use the output to unpack Tokens.
satisfyPT :: (PositionedToken -> Maybe a) -> ASTParser a
satisfyPT f = tokenPrim (show . positionedToken) nextPos f
   where
      nextPos currentPos pt [] = newPos "" (positionedTokenLine pt) (positionedTokenColumn pt)
      nextPos currentPos pt (t:ts) = newPos "" (positionedTokenLine t) (positionedTokenColumn t)

satisfyT :: (Token -> Maybe a) -> ASTParser a
satisfyT f = satisfyPT (f . positionedToken)

-- | Run the given parser in a try block, and if it fails just jump
-- to recovery.
recoverWith :: (ASTParser a) -> (ASTParser ()) -> (ASTParser (Maybe a))
recoverWith p recovery = Just <$> (try p) <|> do
   state <- getParserState
   case testParser (p >> return ()) (stateInput state) of
      Left error -> (modifyState (error:)) >> recovery >> (return Nothing) -- Parser failed hard, remember this fault and do recovery
      Right _    -> Just <$> p -- Parser didn't consume anything, so don't recover, just do p and let parsec continue

testParser :: (ASTParser ()) -> [PositionedToken] -> Either ParseError ()
testParser p s = runParser (p <|> return ()) [] "" s

