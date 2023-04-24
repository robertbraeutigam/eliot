{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines the compiler for the ELIOT language. The compiler
 - takes a set of source paths, and produces a single output program.
 -}

module Compiler(compile) where

import Control.Monad
import Control.Monad.Trans.Class
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, tails, findIndex)
import Text.Parsec.Error
import Text.Parsec.Pos
import System.FilePath
import System.Directory
import GHC.Generics
import Data.Hashable
import FactEngine
import Logging
import Tokens
import AST

-- These data types define all the various stages of the compilation process

data Signal =
     SourcePathDetected FilePath
   | SourceFileDetected FilePath
   | SourceFileRead     FilePath
   | SourceTokenized    FilePath
   | SourceASTCreated   FilePath
   deriving (Eq, Show, Generic, Hashable)

data Fact = 
     SourcePath FilePath                -- A path to some file or directory containing source code
   | SourceFile FilePath                -- A source file that has been detected
   | SourceFileContent FilePath String  -- Contents of a source file
   | SourceTokens FilePath [Token]      -- Tokens read from a source file
   | SourceAST FilePath AST             -- AST of source file
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
         processors = [directoryWalker, fileReader, parseTokensProcessor, parseASTProcessor]

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
   Left parserError -> compilerErrorMsg path (sourceLine $ errorPos parserError) (sourceColumn $ errorPos parserError) (translateParsecErrorMessage (show parserError))
   Right tokens     -> registerFact (SourceTokenized path) (SourceTokens path tokens)
parseTokensProcessor _ = return ()

parseASTProcessor :: CompilerProcessor
parseASTProcessor (SourceTokens path tokens) = case (parseAST tokens) of
   Left parserError -> compilerErrorMsg path (sourceLine $ errorPos parserError) (sourceColumn $ errorPos parserError) (translateParsecErrorMessage (show parserError))
   Right ast        -> debugMsg $ show ast --registerFact (SourceASTCreated path) (SourceAST path ast)
parseASTProcessor _ = return ()

translateParsecErrorMessage msg = "Unexpected " ++ (substringAfterPrefix ",unexpected" msg) ++ ", expected " ++ (substringAfterPrefix ",expected" msg)

substringAfterPrefix prefix str = case dropWhile (not . isInfixOf prefix) (lines str) of
   []     -> "no match in "++(show $ lines str)
   (l:_) -> case afterIndexOf prefix l of
      Nothing  -> "no after index"
      Just ndx -> drop ndx l

afterIndexOf needle haystack = (+(length needle)) <$> (findIndex (isPrefixOf needle) (tails haystack))

