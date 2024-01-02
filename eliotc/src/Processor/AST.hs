{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Build the AST of an ELIOT source. Note, this AST is not yet enriched wih type information
 - nor does it completely assign roles to various tokens.
 -}

module Processor.AST (AST(..), Import(..), FunctionDefinition(..), FunctionBody(..), allImportTokens, Expression(..), parseASTProcessor, SourceASTSignal(..), SourceAST(..)) where

import Data.Hashable
import GHC.Generics
import Tokens
import Data.List (isPrefixOf, intercalate, find)
import Text.Parsec
import Text.Parsec.Pos
import Data.Maybe
import Data.Tree
import Data.Char (isLower, isUpper)
import CompilerProcessor
import Processor.Error
import Processor.Source

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
   functionBody::FunctionBody
}
   deriving (Show, Eq)

data FunctionBody = NativeFunction | NonNativeFunction (Tree Expression)
 deriving (Eq, Show)

data Expression = FunctionApplication PositionedToken
                | NumberLiteral PositionedToken
 deriving (Eq, Show)

data AST = AST { 
   importStatements    :: [Import],
   functionDefinitions :: [FunctionDefinition]
}
   deriving (Show, Eq)

data SourceASTSignal = SourceASTSignal FilePath
   deriving (Eq, Generic)

instance Hashable SourceASTSignal

-- | AST of source file
data SourceAST = SourceAST FilePath AST

parseASTProcessor :: CompilerProcessor
parseASTProcessor v = case getTypedValue v of
   Just (SourceTokens path [])     -> registerAST path (AST [] [])
   Just (SourceTokens path (t:ts)) -> case runParser (positionedRecoveringParseSource t) [] path (t:ts) of
      Left err            -> (compilerErrorAST path (t:ts) err) >> registerAST path (AST [] [])
      Right (errors, ast) -> (mapM_ (compilerErrorAST path (t:ts)) errors) >> registerAST path ast
   _                               -> compileOk

registerAST path ast = registerCompilerFact (SourceASTSignal path) (SourceAST path ast)

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
   fname   <- satisfyAll [isTopLevel, not . isKeyword] <?> "function name"
   nparams <- option [] (inParens $ option [] namedParameters) <?> "function parameters"
   _       <- symbol "=" <?> "function definition equals sign"
   body    <- nativeKeyword <|> (NonNativeFunction <$> expression)
   return $ FunctionDefinition fname nparams body

expression = literal <|> functionApplication

literal = numberLiteral

numberLiteral = do
   nt <- satisfyAll [isNumber] <?> "number literal"
   return $ Node (Processor.AST.NumberLiteral nt) []

functionApplication = do
   fname   <- satisfyAll [isIdentifer] <?> "function name"
   pexps   <- option [] (inParens $ option [] expressionList) <?> "function parameters"
   return $ Node (FunctionApplication fname) pexps

nativeKeyword = do
   _       <- satisfyAll [isKeyword, isContent "native"] <?> "native keyword"
   return NativeFunction

namedParameters = do
   firstParameter      <- satisfyAll [isIdentifer] <?> "first parameter"
   remainingParameters <- many (symbol "," *> (satisfyAll [isIdentifer]))
   return $ firstParameter:remainingParameters

expressionList = do
   firstExpression      <- expression <?> "first expression"
   remainingExpressions <- many (symbol "," *> expression)
   return $ firstExpression:remainingExpressions

inParens = between (symbol "(") (symbol ")")

-- Low level stuff

skipToNextTopLevel SomethingConsumed = skipToNewLineOr eof
skipToNextTopLevel NothingConsumed = return Fallthough

alwaysSkipToNextTopLevel _ = skipToNewLineOr eof

skipToNewLineOr end = anyToken >> core
   where core = (end >> return End) <|> ((notNewLine >> core) <|> return ContinueTrying)

notNewLine = satisfyAll [(not . isTopLevel)]

moduleNameOnSameLineAs pt = satisfyAll [isIdentifer, sameLineAs pt, contentPredicate startsUpperCase] <?> "module name on same line as import"

packageNameOnSameLineAs pt = satisfyAll [isIdentifer, sameLineAs pt, contentPredicate startsLowerCase] <?> "package name on same line as import"

topLevelKeyword name = satisfyAll [isKeyword, isContent name, isTopLevel] <?> ("top level keyword "++(show name))

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

isKeyword (PositionedToken _ _ _ (Keyword _)) = True
isKeyword _                                    = False

isNumber (PositionedToken _ _ _ (Tokens.NumberLiteral _)) = True
isNumber _                                         = False

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
compilerErrorAST fp ts e = case findToken of
      Just (PositionedToken name line column t) -> compilerError name (SourcePosition line column) (SourcePosition line (column + (tokenLength t))) (translateParsecErrorMessage $ show e)
      Nothing -> compilerError fp (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)) (translateParsecErrorMessage $ show e)
   where findToken = find (\(PositionedToken _ line column _) -> line == (sourceLine $ errorPos e) && column == (sourceColumn $ errorPos e)) ts

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

