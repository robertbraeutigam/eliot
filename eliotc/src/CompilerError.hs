{-| Unified error reporting on source code for all phases of the compilation process.
 -}

module CompilerError (SourcePosition(..), CompilerError(..)) where

import Text.Parsec

data SourcePosition = SourcePosition { row::Line, col::Column }
   deriving (Show, Eq)

data CompilerError = CompilerError { errorFrom::SourcePosition, errorTo::SourcePosition, errorMessage::String }
   deriving (Show, Eq)

