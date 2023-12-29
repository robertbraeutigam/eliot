{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
-- | Error reporting based on source content.

module Processor.Error (errorProcessor, compilerError, compilerErrorForFile, CompilerError(..)) where

import CompilerProcessor
import Processor.Source
import GHC.Generics
import Data.Hashable

data CompilerError = CompilerError FilePath SourcePosition SourcePosition String
   deriving (Show, Eq, Generic)

instance Hashable CompilerError

-- | Register a compiler error
compilerError :: FilePath -> SourcePosition -> SourcePosition -> String -> CompilerIO ()
compilerError file fromPos toPos msg = registerCompilerFact errorSignal errorSignal
   where errorSignal = CompilerError file fromPos toPos msg

-- | Indicate an error with a source file in general, without a specific position in the file.
compilerErrorForFile :: FilePath -> String -> CompilerIO ()
compilerErrorForFile file msg = compilerError file (SourcePosition 1 1) (SourcePosition 1 1) msg

-- | Error processor reads all error facts and prints them using the contents of the given file.
errorProcessor :: CompilerProcessor
errorProcessor v = case getTypedValue v of
   Just (CompilerError fp (SourcePosition fromLine fromCol) (SourcePosition toLine toCol) msg) -> do
      source <- getCompilerFact $ SourceFileContentSignal fp
      case source of
         Just (SourceFileContent _ content) -> compilerErrorMsg fp content fromLine fromCol toLine toCol msg
         _                                  -> compileOk
   _                                                                                           -> compileOk

