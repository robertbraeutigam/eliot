{-| Parser processor that parses source files into a sequence of tokens. Note that
 - this parser can not fail. It will by definition parse everything into a token stream.
 -}

module TokenParser(parseTokensProcessor) where

import Text.Parsec
import Text.Parsec.Language
import Data.Char (isAlpha, isSpace)
import Control.Monad
import Tokens
import CompilerFacts
import Logging

parseTokensProcessor :: CompilerProcessor
parseTokensProcessor (SourceFileContent code) = debugMsg $ show $ parse parseTokens "" code
parseTokensProcessor _ = return ()

parseTokens = whiteSpace >> (many ((identifier <|> operator) <* whiteSpace)) <* eof

identifier = do
   firstCharacter <- letter
   restCharacters <- many (alphaNum <|> oneOf "_'")
   return $ Identifier (firstCharacter:restCharacters)

operator = (many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~") >>= (return . Symbol)

-- | Whitespace includes everything from spaces, newlines to comments.
whiteSpace = skipMany $ simpleSpace <|> oneLineComment <|> multiLineComment

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = try (string "//") >> skipMany (satisfy (/= '\n'))

multiLineComment = try (string "/*") >> multiLineCommentBody

multiLineCommentBody = (void $ try (string "*/"))                             -- Comment ended
                   <|> (skipMany1 (satisfy (/= '*')) >> multiLineCommentBody) -- Skip to next *
                   <|> (void $ char '*')                                      -- Skip * if string "*/" didn't match

