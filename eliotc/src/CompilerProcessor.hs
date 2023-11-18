{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ExistentialQuantification #-}
{-| Defines all the types needed to develop a processor for the compiler.
 -}

module CompilerProcessor(Signal(..), Fact(..), CompilerIO, CompilerProcessor, compileOk, CompilerError(..), SourcePosition(..), registerCompilerFact, getCompilerFact, compilerError, infoMsg, errorMsg, debugMsg, compilerErrorMsg, compilerErrorForFile, compilerErrorForTokens) where

import GHC.Generics
import Data.Hashable
import Data.Dynamic
import qualified Data.ByteString as ByteString
import Control.Monad.Trans.Reader
import qualified Logging
import qualified Data.Map as Map
import Engine.FactEngine
import Tokens
import AST
import FAST
import Module
import Generator

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
     InitSignal
   | SourcePathSignal                 FilePath
   | SourceFileSignal                 FilePath
   | SourceFileContentSignal          FilePath
   | SourceTokensSignal               FilePath
   | SourceASTSignal                  FilePath
   | CompilerErrorSignal              CompilerError
   | ModuleFunctionNamesSignal        ModuleName
   | FunctionCompilationUnitSignal    FunctionFQN
   | CompiledFunctionSignal           FunctionFQN
   | GenerateMainSignal
   | TargetBinaryGeneratedSignal      TargetPlatform
   | PlatformGeneratedFunctionSignal  TargetPlatform FunctionFQN
   deriving (Eq, Show, Generic, Hashable)

-- | Facts registered into the fact engine.
data Fact = 
     Init                                                                                  -- Called to do startup logic of processors
   | SourcePath                FilePath                                                    -- A path to some file or directory containing source code
   | SourceFile                FilePath                                                    -- A source file that has been detected
   | SourceFileContent         FilePath String                                             -- Contents of a source file
   | SourceTokens              FilePath [PositionedToken]                                  -- Tokens read from a source file
   | SourceAST                 FilePath AST                                                -- AST of source file
   | CompilerErrorFact         CompilerError
   | ModuleFunctionNames       ModuleName [String]                                         -- A list of functions in the module
   | FunctionCompilationUnit   FunctionFQN (Map.Map String FunctionFQN) FunctionDefinition -- A function ready to be compiled and type-checked
   | CompiledFunction          FunctionFQN (Maybe FunctionBody)                            -- A compiled (type-checked) correct function body, of there is no body, that's a native function
   | GenerateMain              TargetPlatform FunctionFQN                                  -- Ask processors to generate for this main function and target platform
   | TargetBinaryGenerated     TargetPlatform ModuleName ByteString.ByteString             -- The target platform produced the compiled version of the source code
   | PlatformGeneratedFunction TargetPlatform FunctionFQN Dynamic                          -- Generated some platform specific output for the given function

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
getCompilerFact :: Signal -> CompilerIO (Maybe Fact)
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
         toLastToken    = case pos $ last pts of
            (SourcePosition line column) -> SourcePosition line (column + (length $ positionedTokenContent (last pts)))
         pos (PositionedToken _ line column _) = SourcePosition line column

-- | Logging
errorMsg :: String -> CompilerIO ()
errorMsg msg = withReaderT fst $ Logging.errorMsg msg

debugMsg :: String -> CompilerIO ()
debugMsg msg = withReaderT fst $ Logging.debugMsg msg

infoMsg :: String -> CompilerIO ()
infoMsg msg = withReaderT fst $ Logging.infoMsg msg

compilerErrorMsg :: FilePath -> String -> Int -> Int -> Int -> Int -> String -> CompilerIO ()
compilerErrorMsg filePath content fRow fCol tRow tCol msg = withReaderT fst $ Logging.compilerErrorMsg filePath content fRow fCol tRow tCol msg

