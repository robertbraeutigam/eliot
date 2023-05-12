{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module Processor.ASTProcessor (parseAST) where

import Data.List (isPrefixOf, intercalate, find)
import Text.Parsec
import Text.Parsec.Pos
import Data.Maybe
import Data.Char (isLower, isUpper)
import Tokens
import CompilerProcessor
import AST

type ASTParser = Parsec [PositionedToken] [ParseError]

-- | Run the parser with all features
parseAST :: FilePath -> [PositionedToken] -> ([CompilerError], AST)
parseAST fp [] = ([], AST [] [])
parseAST fp (t:ts) = case run of
      (es, ast) -> ((map (translateASTError fp (t:ts)) es), ast)
   where run = either (\e -> ([e], AST [] [])) id $ runParser (positionedRecoveringParseSource t) [] "" (t:ts)

-- | Set the source position of the first token explicitly before the parsing starts.
positionedRecoveringParseSource t =
   (setPosition $ newPos "" (positionedTokenLine t) (positionedTokenColumn t)) >> recoveringParseSource

-- | Recover from all errors of the parser and store the error in the user state instead.
recoveringParseSource = do
   ast    <- parseSource
   errors <- getState
   return (reverse errors, ast)

parseSource = do
   imps <- many $ importStatement `recoverWith` skipToNextTopLevel
   funs <- many $ functionStatement `recoverWith` alwaysSkipToNextTopLevel
   return $ AST (catMaybes imps) (catMaybes funs)

importStatement = do
   kyw  <- topLevelKeyword "import"
   pkgs <- many (packageName <* (symbol "."))
   modn <- moduleName
   return $ Import kyw pkgs modn

functionStatement = do
   firstDef <- satisfyAll [isTopLevel] <?> "top level function definition"
   restDefs <- many $ satisfyAll [contentPredicate (/="=")]
   _        <- symbol "=" <?> "function definition equals sign"
   body     <- many $ notNewLine
   return $ FunctionDefinition (firstDef:restDefs) body

-- Low level stuff

skipToNextTopLevel SomethingConsumed = skipToNewLineOr eof
skipToNextTopLevel NothingConsumed = return Fallthough

alwaysSkipToNextTopLevel _ = skipToNewLineOr eof

skipToNewLineOr end = anyToken >> core
   where core = (end >> return End) <|> ((notNewLine >> core) <|> return ContinueTrying)

notNewLine = satisfyAll [(not . isTopLevel)]

moduleName = satisfyAll [isIdentifer, contentPredicate startsUpperCase] <?> "module name"

packageName = satisfyAll [isIdentifer, contentPredicate startsLowerCase] <?> "package name"

keyword name = satisfyAll [isIdentifer, isContent name] <?> ("keyword "++(show name))

topLevelKeyword name = satisfyAll [isIdentifer, isContent name, isTopLevel] <?> ("top level keyword "++(show name))

symbol name = satisfyAll [isSymbol, isContent name] <?> ("symbol '" ++ name ++ "'")

startsUpperCase ""     = False
startsUpperCase (c:_) = isUpper c 

startsLowerCase ""     = False
startsLowerCase (c:_) = isLower c 

isContent content = contentPredicate (== content)

isIdentifer (PositionedToken _ _ (Identifier _)) = True
isIdentifer _                                    = False

isSymbol (PositionedToken _ _ (Symbol _)) = True
isSymbol _                                = False

isTopLevel (PositionedToken _ column _) = column == 1

contentPredicate f = f . tokenContent

-- | Using the primitive token function to maybe parse a token and produce the token.
satisfyPT :: (PositionedToken -> Bool) -> ASTParser PositionedToken
satisfyPT f = tokenPrim (show . positionedToken) nextPos nextToken
   where
      nextPos _ pt [] = newPos "" (positionedTokenLine pt) (positionedTokenColumn pt)
      nextPos _ _ (t:_) = newPos "" (positionedTokenLine t) (positionedTokenColumn t)
      nextToken pt = if f pt then Just pt else Nothing

satisfyAll :: [PositionedToken -> Bool] -> ASTParser PositionedToken
satisfyAll ps = satisfyPT $ (\pt -> all ($ pt) ps)

tokenContent (PositionedToken _ _ (Identifier content)) = content
tokenContent (PositionedToken _ _ (Symbol content)) = content

-- | Recover the given parser if it fails, whether it consumed any input or not.
-- Retry the parser if the recovery returns True, otherwise fail to recovery.
recoverWith :: (ASTParser a) -> (RecoveryMode -> ASTParser RecoveryResult) -> (ASTParser (Maybe a))
recoverWith p recovery  = do
   state <- getParserState
   case runParser (positionedP state) [] "" (stateInput state) of
      Left parserError -> recovery (mode parserError state) >>= (\result -> case result of
          ContinueTrying    -> (modifyState (parserError:)) >> recoverWith p recovery
          End               -> (modifyState (parserError:)) >> return Nothing
          Fallthough        -> Just <$> p)
      Right _          -> Just <$> p -- p was successful in test run, so run it for real
   where positionedP state = (setPosition $ statePos state) >> p
         mode e s = if errorPos e == statePos s then NothingConsumed else SomethingConsumed
         
data RecoveryMode = NothingConsumed | SomethingConsumed

data RecoveryResult = ContinueTrying | End | Fallthough
   deriving (Eq, Show)

-- | Translate error messages from parsec to readable compiler messages.
translateASTError fp ts e = case findToken of
      Just t  -> CompilerError fp (SourcePosition (positionedTokenLine t) (positionedTokenColumn t)) (SourcePosition (positionedTokenLine t) ((positionedTokenColumn t) + (tokenLength $ positionedToken t))) (translateParsecErrorMessage $ show e)
      Nothing -> CompilerError fp (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (translateParsecErrorMessage $ show e)
   where findToken = find (\t -> (positionedTokenLine t) == (sourceLine $ errorPos e) && (positionedTokenColumn t) == (sourceColumn $ errorPos e)) ts

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

