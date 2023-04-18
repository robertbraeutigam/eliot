{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| This is the list of all the Signals and Facts the compiler produces,
 - i.e. all that can be consumed or produced by all the processors.
 -}

module CompilerFacts (Signal(..), Fact(..), CompilerProcessor) where

import GHC.Generics
import Data.Hashable
import FactEngine

data Signal = SourcePathArgument String
   deriving (Eq, Show, Generic, Hashable)

data Fact = SourcePath String
   deriving (Eq, Show)

type CompilerProcessor = FactProcessor Signal Fact
