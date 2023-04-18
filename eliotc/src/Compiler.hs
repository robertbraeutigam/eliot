{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

import Control.Monad.Extra
import Data.Maybe
import FactEngine
import CompilerFacts
import Logging

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = whenM (isNothing <$> resolveFacts [] sourcePathFacts) $ errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathArgument s, SourcePath s)) paths

