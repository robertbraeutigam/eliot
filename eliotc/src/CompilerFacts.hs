{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| This is the list of all the Signals and Facts the compiler produces,
 - i.e. all that can be consumed or produced by all the processors.
 -}

module CompilerFacts (Signal(..), Fact(..), CompilerProcessor) where

import GHC.Generics
import Data.Hashable
import FactEngine

data Signal =
     SourcePathDetected FilePath
   | SourceFileDetected FilePath
   | SourceFileRead     FilePath
   deriving (Eq, Show, Generic, Hashable)

data Fact = 
     SourcePath FilePath                -- A path to some file or directory containing source code
   | SourceFile FilePath                -- A source file that has been detected
   | SourceFileContent String           -- Contents of a source file
   deriving (Eq, Show)

type CompilerProcessor = FactProcessor Signal Fact
