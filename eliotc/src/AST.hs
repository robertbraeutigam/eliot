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
   imps <- many $ importStatement `recoverWith` skipToNewLineOrEof
   return $ AST (catMaybes imps)

importStatement = do
   _    <- keyword "import"
   pkgs <- many (packageName <* (symbol "."))
   modn <- moduleName
   return $ Import pkgs modn

-- Low level stuff

skipToNewLineOrEof = anyToken >> core
   where core = (eof >> return False) <|> ((notNewLine >> core) <|> return True)

notNewLine = satisfyPT (\pt -> if (positionedTokenColumn pt) /= 1 then Just () else Nothing)

moduleName = satisfyT (\t -> case t of
   Identifier identifier@(c:_) -> if isUpper c then Just identifier else Nothing
   _                   -> Nothing) <?> "module name"

packageName = satisfyT (\t -> case t of
   Identifier identifier@(c:_) -> if isLower c then Just identifier else Nothing
   _                   -> Nothing) <?> "package name"

keyword name = satisfyT (\t -> case t of
   Identifier identifier -> if identifier == name then Just identifier else Nothing
   _             -> Nothing) <?> ("keyword "++(show name))

symbol name = satisfyT (\t -> case t of
   Symbol identifier -> if identifier == name then Just identifier else Nothing
   _         -> Nothing) <?> ("symbol '" ++ name ++ "'")


type ASTParser = Parsec [PositionedToken] [ParseError]

-- | Using the primitive token function to maybe parse a token and produce
-- an output. We use the output to unpack Tokens.
satisfyPT :: (PositionedToken -> Maybe a) -> ASTParser a
satisfyPT f = tokenPrim (show . positionedToken) nextPos f
   where
      nextPos _ pt [] = newPos "" (positionedTokenLine pt) (positionedTokenColumn pt)
      nextPos _ _ (t:_) = newPos "" (positionedTokenLine t) (positionedTokenColumn t)

satisfyT :: (Token -> Maybe a) -> ASTParser a
satisfyT f = satisfyPT (f . positionedToken)

-- | Recover the given parser if it fails, whether it consumed any input or not.
-- Retry the parser if the recovery returns True, otherwise fail to recovery.
recoverWith :: (ASTParser a) -> (ASTParser Bool) -> (ASTParser (Maybe a))
recoverWith p recovery  = do
   state <- getParserState
   case runParser p [] "" (stateInput state) of
      Left parserError -> (modifyState (parserError:)) >> recovery >>= (\b -> if b then recoverWith p recovery else return Nothing)
      Right _          -> Just <$> p -- p was successful in test run, so run it for real

