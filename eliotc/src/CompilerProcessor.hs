{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines all the types needed to develop a processor for the compiler.
 -}

module CompilerProcessor(Signal(..), Fact(..), CompilerIO, CompilerProcessor, compileOk, CompilerError(..), SourcePosition(..)) where

import Text.Parsec
import GHC.Generics
import Data.Hashable
import Engine.FactEngine
import Tokens
import AST

data SourcePosition = SourcePosition { row::Line, col::Column }
   deriving (Show, Eq)

data CompilerError = CompilerError { errorFrom::SourcePosition, errorTo::SourcePosition, errorMessage::String }
   deriving (Show, Eq)

-- | Signals registered into the fact engine.
data Signal =
     SourcePathDetected FilePath
   | SourceFileDetected FilePath
   | SourceFileRead     FilePath
   | SourceTokenized    FilePath
   | SourceASTCreated   FilePath
   deriving (Eq, Show, Generic, Hashable)

-- | Facts registered into the fact engine.
data Fact = 
     SourcePath FilePath                          -- A path to some file or directory containing source code
   | SourceFile FilePath                          -- A source file that has been detected
   | SourceFileContent FilePath String            -- Contents of a source file
   | SourceTokens FilePath [PositionedToken]      -- Tokens read from a source file
   | SourceAST FilePath AST             -- AST of source file
   deriving (Eq, Show)

-- | A computation running in the compiler. It either produces some value
-- or returns a list of errors to be displayed.
type CompilerIO a = FactsIO Signal Fact ([CompilerError], a)

-- | A compiler process reacts to a fact and runs a CompilerIO computation.
type CompilerProcessor = Fact -> CompilerIO ()

-- | Return no errors an void from a compiler processor.
compileOk :: CompilerIO ()
compileOk = return $ ([], ())

