{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

import Control.Monad
import Control.Monad.Trans.Class
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Text.Parsec.Error
import Text.Parsec.Pos
import System.FilePath
import System.Directory
import GHC.Generics
import Data.Hashable
import FactEngine
import Logging
import Tokens

-- These data types define all the various stages of the compilation process

data Signal =
     SourcePathDetected FilePath
   | SourceFileDetected FilePath
   | SourceFileRead     FilePath
   | SourceTokenized    FilePath
   deriving (Eq, Show, Generic, Hashable)

data Fact = 
     SourcePath FilePath                -- A path to some file or directory containing source code
   | SourceFile FilePath                -- A source file that has been detected
   | SourceFileContent FilePath String  -- Contents of a source file
   | SourceTokens FilePath [Token]      -- Tokens read from a source file
   deriving (Eq, Show)

type CompilerProcessor = FactProcessor Signal Fact

-- | Run the compiler on the given source paths.
compile :: [String] -> IO ()
compile [] = errorMsg "There were no source paths given. Please supply at least one directory with ELIOT sources."
compile paths = do
   facts <- resolveFacts processors sourcePathFacts
   case facts of
      Just(allFacts) -> debugMsg $ "Calculated facts " ++ (show (map fst allFacts))
      Nothing        -> errorMsg "Compiler terminated with errors. See previous errors for details."
   where sourcePathFacts = map (\s -> (SourcePathDetected s, SourcePath s)) paths
         processors = [directoryWalker, fileReader, parseTokensProcessor]

-- From here on are the processors for the compilation process

directoryWalker :: CompilerProcessor
directoryWalker (SourcePath path) = do
   isFile      <- lift $ doesFileExist path
   isDirectory <- lift $ doesDirectoryExist path
   when isFile       $ registerFact (SourceFileDetected path) (SourceFile path) 
   when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerFact . SourcePathDetected <*> SourcePath) . (path </>))
   when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
directoryWalker _ = return ()

fileReader :: CompilerProcessor
fileReader (SourceFile path)
   | ".els" `isSuffixOf` path = (lift $ readFile path) >>= (registerFact (SourceFileRead path) . (SourceFileContent path))
   | otherwise                = debugMsg $ "Ignoring source file because not ending in '.els': " ++ path
fileReader _ = return ()

parseTokensProcessor :: CompilerProcessor
parseTokensProcessor (SourceFileContent path code) = case (parseTokens code) of
   Left parserError -> compilerErrorMsg path (sourceLine $ errorPos parserError) (sourceColumn $ errorPos parserError) (translateErrorMessage (show parserError))
   Right tokens     -> registerFact (SourceTokenized path) (SourceTokens path tokens)
   where translateErrorMessage msg -- This is a little bit of a hack, but don't know a better way at this time
           | "block comment" `isInfixOf` msg   = "Block comment was not closed, end of file reached. Please close block comment with '*/'."
           | "legal character" `isInfixOf` msg = "Illegal character in source code."
           | otherwise                         = "Unknown token parsing failure: " ++ (singleLine msg)
         singleLine msg = map (\c -> if c == '\n' then ',' else c) msg
parseTokensProcessor _ = return ()

