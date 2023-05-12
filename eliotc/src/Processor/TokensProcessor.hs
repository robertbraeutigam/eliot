{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Tokens of the ELIOT language. Tokens are the next building block above characters the language
 - is made out of.
 -}

module Processor.TokensProcessor (parseTokens) where

import Text.Parsec
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Control.Monad
import CompilerProcessor
import Tokens

parseTokens :: FilePath -> String -> Either CompilerError [PositionedToken]
parseTokens fp code = case parse (whiteSpace >> (many anyTokenLexeme) <* eof) fp code of
   Left e -> Left $ translateTokenizerError fp e
   Right ts -> Right ts

anyTokenLexeme = ((identifier <|> symbol) <* whiteSpace) <?> "legal character"

identifier = do
   firstCharacter <- letter
   restCharacters <- many (alphaNum <|> oneOf "_'")
   pos            <- getPosition
   return $ PositionedToken (sourceLine pos) ((sourceColumn pos)-1-(length restCharacters)) (Identifier (firstCharacter:restCharacters))

symbol = (do
   sym <- (many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~;")
   pos <- getPosition
   return $ PositionedToken (sourceLine pos) ((sourceColumn pos)-(length sym)) (Symbol sym)) <?> "operator"

-- | Whitespace includes everything from spaces, newlines to comments.
whiteSpace = skipMany $ simpleSpace <|> ((oneLineComment <|> multiLineComment) <?> "comment")

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = try (string "//") >> skipMany (satisfy (/= '\n'))

multiLineComment = try (string "/*") >> multiLineCommentBody

multiLineCommentBody = ((void $ try (string "*/"))                             -- Comment ended
                   <|> (skipMany1 (satisfy (/= '*')) >> multiLineCommentBody) -- Skip to next *
                   <|> ((void $ char '*') >> multiLineCommentBody))            -- Skip * if string "*/" didn't match
                   <?> "closing '*/' of block comment"

-- Translate errors

translateTokenizerError fp e = CompilerError fp pos pos (translateParsecErrorMessage $ show e)
   where pos = SourcePosition (sourceLine $ errorPos e) (sourceColumn $ errorPos e)

translateParsecErrorMessage msg = "Parser error, " ++ (intercalate ", " $ filter (\l -> (isPrefixOf "unexpected" l) || (isPrefixOf "expecting" l))  (lines msg)) ++ "."

