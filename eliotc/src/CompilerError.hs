{-| Unified error reporting on source code for all phases of the compilation process.
 -}

module CompilerError (SourcePosition(..), CompilerError(..)) where

import Text.Parsec

data SourcePosition = SourcePosition { row::Line, col::Column }

data CompilerError = CompilerError { errorFrom::SourcePosition, errorTo::SourcePosition, errorMessage::String }

