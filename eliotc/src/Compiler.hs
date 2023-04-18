{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

import Control.Monad.Extra
import Data.Maybe
import System.Environment
import System.IO
import FactEngine
import CompilerFacts

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = fail "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = whenM (isNothing <$> resolveFacts [] sourcePathFacts) $ do
   progName <- getProgName
   hPutStrLn stderr (progName ++ ":compiler terminated with error")
   where sourcePathFacts = map (\s -> (SourcePathArgument s, SourcePath s)) paths

