{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Defines all the types needed to develop a processor for the compiler.
 -}

module CompilerProcessor(Signal(..), Fact(..), CompilerIO, CompilerProcessor, compileOk, CompilerError(..), SourcePosition(..), registerCompilerFact, getCompilerFact, compilerError, errorMsg, debugMsg, compilerErrorMsg, compilerErrorForFile, compilerErrorForTokens) where

import GHC.Generics
import Data.Hashable
import Control.Monad.Trans.Reader
import qualified Logging
import Engine.FactEngine
import Tokens
import AST
import FAST

type SourceLine = Int

type SourceColumn = Int

data SourcePosition = SourcePosition { row::SourceLine, col::SourceColumn }
   deriving (Show, Eq)

instance Hashable SourcePosition where
  hashWithSalt salt (SourcePosition l c) = hashWithSalt salt (l, c)

data CompilerError = CompilerError { errorFile::FilePath, errorFrom::SourcePosition, errorTo::SourcePosition, errorMessage::String }
   deriving (Show, Eq)

instance Hashable CompilerError where
  hashWithSalt salt (CompilerError file ef et em) = hashWithSalt salt (file, ef, et, em)

-- | Signals registered into the fact engine.
data Signal =
     SourcePathDetected  FilePath
   | SourceFileDetected  FilePath
   | SourceFileRead      FilePath
   | SourceTokenized     FilePath
   | SourceASTCreated    FilePath
   | CompilerErrorSignal CompilerError
   | ModuleFASTCreated   ModuleName
   deriving (Eq, Show, Generic, Hashable)

-- | Facts registered into the fact engine.
data Fact = 
     SourcePath FilePath                          -- A path to some file or directory containing source code
   | SourceFile FilePath                          -- A source file that has been detected
   | SourceFileContent FilePath String            -- Contents of a source file
   | SourceTokens FilePath [PositionedToken]      -- Tokens read from a source file
   | SourceAST FilePath AST                       -- AST of source file
   | CompilerErrorFact CompilerError
   | ModuleFAST Module                            -- The Module FAST description
   deriving (Eq, Show)

-- | A computation running in the compiler. This computation interacts
-- with facts, may get and register them, and potentially produces errors during
-- processing. The errors are not short-circuited, the processor may produce
-- multiple errors and still produce some output.
type CompilerIO = ReaderT (Logging.Logger, (FactEngine Signal Fact)) IO

-- | A compiler process reacts to a fact and runs a CompilerIO computation.
type CompilerProcessor = Fact -> CompilerIO ()

-- | Return no errors an void from a compiler processor.
compileOk :: CompilerIO ()
compileOk = return ()

-- | Register a fact into the compiler engine.
registerCompilerFact :: Signal -> Fact -> CompilerIO ()
registerCompilerFact s f = withReaderT snd $ registerFact s f

-- | Get a fact from the compiler engine. This will potentially block
-- until the fact becomes available.
getCompilerFact :: Signal -> CompilerIO Fact
getCompilerFact s = withReaderT snd $ getFact s

-- | Generate a compiler error.
compilerError :: CompilerError -> CompilerIO ()
compilerError e = registerCompilerFact (CompilerErrorSignal e) (CompilerErrorFact e)

compilerErrorForFile :: FilePath -> String -> CompilerIO ()
compilerErrorForFile file msg = compilerError $ CompilerError file (SourcePosition 1 1) (SourcePosition 1 1) msg

compilerErrorForTokens :: [PositionedToken] -> String -> CompilerIO ()
compilerErrorForTokens [] _ = error "Compiler error was invoked on no tokens."
compilerErrorForTokens pts@((PositionedToken file _ _ _):_) msg = compilerError $ CompilerError file fromFirstToken toLastToken msg
   where fromFirstToken = pos $ head pts
         toLastToken    = pos $ last pts
         pos (PositionedToken _ line column _) = SourcePosition line column

-- | Logging
errorMsg :: String -> CompilerIO ()
errorMsg msg = withReaderT fst $ Logging.errorMsg msg

debugMsg :: String -> CompilerIO ()
debugMsg msg = withReaderT fst $ Logging.debugMsg msg

compilerErrorMsg :: FilePath -> String -> Int -> Int -> Int -> Int -> String -> CompilerIO ()
compilerErrorMsg filePath content fRow fCol tRow tCol msg = withReaderT fst $ Logging.compilerErrorMsg filePath content fRow fCol tRow tCol msg

