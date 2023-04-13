{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = fail "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = pure ()

