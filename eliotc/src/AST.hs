{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module AST (AST(..), Import(..), parseAST) where

import Data.List (isPrefixOf, intercalate, find)
import Text.Parsec
import Text.Parsec.Pos
import Data.Maybe
import Data.Char (isLower, isUpper)
import Tokens
import CompilerError

data Import = Import [String] String  -- Import statement with pacakges and module name
   deriving (Show, Eq)

data AST = AST { 
   importStatements :: [Import]
}
   deriving (Show, Eq)

type ASTParser = Parsec [PositionedToken] [ParseError]

-- | Run the parser with all features
parseAST :: [PositionedToken] -> ([CompilerError], AST)
parseAST [] = ([], AST [])
parseAST (t:ts) = case run of
      (es, ast) -> ((map (translateASTError (t:ts)) es), ast)
   where run = either (\e -> ([e], AST [])) id $ runParser (positionedRecoveringParseSource t) [] "" (t:ts)

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

moduleName = satisfyAll [isIdentifer, contentPredicate startsUpperCase] <?> "module name"

packageName = satisfyAll [isIdentifer, contentPredicate startsLowerCase] <?> "package name"

keyword name = satisfyAll [isIdentifer, isContent name] <?> ("keyword "++(show name))

symbol name = satisfyAll [isSymbol, isContent name] <?> ("symbol '" ++ name ++ "'")

startsUpperCase ""     = False
startsUpperCase (c:cs) = isUpper c 

startsLowerCase ""     = False
startsLowerCase (c:cs) = isLower c 

isContent content = contentPredicate (== content)

isIdentifer (PositionedToken _ _ (Identifier _)) = True
isIdentifer _                                    = False

isSymbol (PositionedToken _ _ (Symbol _)) = True
isSymbol _                                = False

contentPredicate f = f . tokenContent

-- | Using the primitive token function to maybe parse a token and produce
-- an output. We use the output to unpack Tokens.
satisfyPT :: (PositionedToken -> Maybe a) -> ASTParser a
satisfyPT f = tokenPrim (show . positionedToken) nextPos f
   where
      nextPos _ pt [] = newPos "" (positionedTokenLine pt) (positionedTokenColumn pt)
      nextPos _ _ (t:_) = newPos "" (positionedTokenLine t) (positionedTokenColumn t)

satisfyAll :: [PositionedToken -> Bool] -> ASTParser String
satisfyAll ps = satisfyPT (\pt -> if (applyPredicates pt) then Just (tokenContent pt) else Nothing)
   where applyPredicates pt = all (\p -> p pt) ps

tokenContent (PositionedToken _ _ (Identifier content)) = content
tokenContent (PositionedToken _ _ (Symbol content)) = content

-- | Recover the given parser if it fails, whether it consumed any input or not.
-- Retry the parser if the recovery returns True, otherwise fail to recovery.
recoverWith :: (ASTParser a) -> (ASTParser Bool) -> (ASTParser (Maybe a))
recoverWith p recovery  = do
   state <- getParserState
   case runParser (positionedP state) [] "" (stateInput state) of
      Left parserError -> (modifyState (parserError:)) >> recovery >>= (\b -> if b then recoverWith p recovery else return Nothing)
      Right _          -> Just <$> p -- p was successful in test run, so run it for real
   where positionedP state = (setPosition $ statePos state) >> p

-- | Translate error messages from parsec to readable compiler messages.
translateASTError ts e = case findToken of
      Just t  -> CompilerError (SourcePosition (positionedTokenLine t) (positionedTokenColumn t)) (SourcePosition (positionedTokenLine t) ((positionedTokenColumn t) + (tokenLength $ positionedToken t))) (translateParsecErrorMessage $ show e)
      Nothing -> CompilerError (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (translateParsecErrorMessage $ show e)
   where findToken = find (\t -> (positionedTokenLine t) == (sourceLine $ errorPos e) && (positionedTokenColumn t) == (sourceColumn $ errorPos e)) ts

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

