{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

import FactEngine
import CompilerFacts
import Logging
import Files
import Parser

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = do
   facts <- resolveFacts processors sourcePathFacts
   case facts of
      Just(allFacts) -> debugMsg $ "Calculated facts " ++ (show (map fst allFacts))
      Nothing        -> errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathDetected s, SourcePath s)) paths
         processors = [directoryWalker, fileReader, parserProcessor]
