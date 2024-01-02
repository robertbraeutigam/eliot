{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ExistentialQuantification #-}
{-| Defines all the types needed to develop a processor for the compiler.
 -}

module CompilerProcessor(InitSignal(..), Init(..), CompilerIO, CompilerProcessor, compileOk, registerCompilerFact, getCompilerFact, infoMsg, errorMsg, debugMsg, compilerErrorMsg, getTypedValue) where

import GHC.Generics
import Data.Hashable
import Data.Dynamic
import Control.Monad.Trans.Reader
import qualified Logging
import Engine.DynamicFactEngine

data InitSignal = InitSignal
   deriving (Eq, Generic)
instance Hashable InitSignal

-- | A fact that will be inserted at the very beginning of the compile process. This signal should kick-off the compile process.
data Init = Init

-- | A computation running in the compiler. This computation interacts
-- with facts, may get and register them, and potentially produces errors during
-- processing. The errors are not short-circuited, the processor may produce
-- multiple errors and still produce some output.
type CompilerIO = ReaderT (Logging.Logger, DynamicFactEngine) IO

-- | A compiler process reacts to a fact and runs a CompilerIO computation.
type CompilerProcessor = DynamicValue -> CompilerIO ()

-- | Return no errors an void from a compiler processor.
compileOk :: CompilerIO ()
compileOk = return ()

-- | Get the typed value from the dynamic value.
getTypedValue :: (Typeable v) => DynamicValue -> Maybe v
getTypedValue v = fromDynValue v

-- | Register a fact into the compiler engine.
registerCompilerFact :: (Hashable s, Typeable s, Typeable f) => s -> f -> CompilerIO ()
registerCompilerFact s f = withReaderT snd $ registerFact s f

-- | Get a fact from the compiler engine. This will potentially block
-- until the fact becomes available.
getCompilerFact :: (Hashable s, Typeable s, Typeable f) => s -> CompilerIO (Maybe f)
getCompilerFact s = withReaderT snd $ getFact s

-- | Logging
errorMsg :: String -> CompilerIO ()
errorMsg msg = withReaderT fst $ Logging.errorMsg msg

debugMsg :: String -> CompilerIO ()
debugMsg msg = withReaderT fst $ Logging.debugMsg msg

infoMsg :: String -> CompilerIO ()
infoMsg msg = withReaderT fst $ Logging.infoMsg msg

compilerErrorMsg :: FilePath -> String -> Int -> Int -> Int -> Int -> String -> CompilerIO ()
compilerErrorMsg filePath content fRow fCol tRow tCol msg = withReaderT fst $ Logging.compilerErrorMsg filePath content fRow fCol tRow tCol msg

