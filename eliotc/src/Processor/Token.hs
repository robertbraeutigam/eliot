module Processor.Token (compilerErrorForTokens) where

import Tokens
import Processor.Error
import Processor.Source
import CompilerProcessor

compilerErrorForTokens :: [PositionedToken] -> String -> CompilerIO ()
compilerErrorForTokens [] _ = error "Compiler error was invoked on no tokens."
compilerErrorForTokens pts@((PositionedToken file _ _ _):_) msg = compilerError file fromFirstToken toLastToken msg
   where fromFirstToken = pos $ head pts
         toLastToken    = case pos $ last pts of
            (SourcePosition line column) -> SourcePosition line (column + (length $ positionedTokenContent (last pts)))
         pos (PositionedToken _ line column _) = SourcePosition line column

