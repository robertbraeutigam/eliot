{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module Processor.ASTProcessor (parseASTProcessor) where

import Data.List (isPrefixOf, intercalate, find)
import Text.Parsec
import Text.Parsec.Pos
import Data.Maybe
import Data.Char (isLower, isUpper)
import Tokens
import CompilerProcessor
import AST

parseASTProcessor :: CompilerProcessor
parseASTProcessor (SourceTokens path [])     = registerAST path (AST [] [])
parseASTProcessor (SourceTokens path (t:ts)) = case runParser (positionedRecoveringParseSource t) [] path (t:ts) of
   Left err            -> (compilerError (translateASTError path (t:ts) err)) >> registerAST path (AST [] [])
   Right (errors, ast) -> (mapM_ compilerError (translateASTError path (t:ts) <$> errors)) >> registerAST path ast
parseASTProcessor _ = compileOk

registerAST path ast = registerCompilerFact (SourceASTCreated path) (SourceAST path ast)

type ASTParser = Parsec [PositionedToken] [ParseError]

-- | Set the source position of the first token explicitly before the parsing starts.
positionedRecoveringParseSource (PositionedToken name line column _) =
   (setPosition $ newPos name line column) >> recoveringParseSource

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
   pkgs <- many (packageNameOnSameLineAs kyw <* (symbol "."))
   modn <- moduleNameOnSameLineAs kyw
   return $ Import kyw pkgs modn

functionStatement = do
   firstDef <- satisfyAll [isTopLevel] <?> "top level function definition"
   restDefs <- many $ satisfyAll [contentPredicate (/="="), sameLineAs firstDef]
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

moduleNameOnSameLineAs pt = satisfyAll [isIdentifer, sameLineAs pt, contentPredicate startsUpperCase] <?> "module name on same line as import"

packageNameOnSameLineAs pt = satisfyAll [isIdentifer, sameLineAs pt, contentPredicate startsLowerCase] <?> "package name on same line as import"

topLevelKeyword name = satisfyAll [isIdentifer, isContent name, isTopLevel] <?> ("top level keyword "++(show name))

symbol name = satisfyAll [isSymbol, isContent name] <?> ("symbol '" ++ name ++ "'")

startsUpperCase ""     = False
startsUpperCase (c:_) = isUpper c 

startsLowerCase ""     = False
startsLowerCase (c:_) = isLower c 

isContent content = contentPredicate (== content)

isIdentifer (PositionedToken _ _ _ (Identifier _)) = True
isIdentifer _                                    = False

isSymbol (PositionedToken _ _ _ (Symbol _)) = True
isSymbol _                                = False

isTopLevel (PositionedToken _ _ column _) = column == 1

sameLineAs (PositionedToken _ line1 _ _) (PositionedToken _ line2 _ _) = line1 == line2

contentPredicate f = f . positionedTokenContent

-- | Using the primitive token function to maybe parse a token and produce the token.
satisfyPT :: (PositionedToken -> Bool) -> ASTParser PositionedToken
satisfyPT f = tokenPrim show nextPos nextToken
   where
      nextPos _ (PositionedToken name line column _) [] = newPos name line column
      nextPos _ _ ((PositionedToken name line column _):_) = newPos name line column
      nextToken pt = if f pt then Just pt else Nothing

satisfyAll :: [PositionedToken -> Bool] -> ASTParser PositionedToken
satisfyAll ps = satisfyPT $ (\pt -> all ($ pt) ps)

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
      Just (PositionedToken name line column t) -> CompilerError name (SourcePosition line column) (SourcePosition line (column + (tokenLength t))) (translateParsecErrorMessage $ show e)
      Nothing -> CompilerError fp (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (translateParsecErrorMessage $ show e)
   where findToken = find (\(PositionedToken _ line column _) -> line == (sourceLine $ errorPos e) && column == (sourceColumn $ errorPos e)) ts

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

