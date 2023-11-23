{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVRGeneratorProcessor (parseAVRGenerate) where

import CompilerProcessor
import Module
import Generator
import Data.Dynamic
import Control.Applicative
import qualified Data.ByteString as ByteString
import FAST

parseAVRGenerate :: CompilerProcessor
parseAVRGenerate (GenerateMain tp@(TargetPlatform "attiny424") ffqn@(FunctionFQN moduleName _)) = do
   bytesMaybe <- collectBytesFrom tp ffqn
   case bytesMaybe of
      Just bytes -> registerCompilerFact (TargetBinaryGeneratedSignal tp) (TargetBinaryGenerated tp moduleName bytes)
      Nothing    -> compileOk
-- TODO: The platform should come from the signal to compile, later from the optimizer, or whatever, it is hardcoded here
parseAVRGenerate (CompiledFunction ffqn fbody) = transformToBytes (TargetPlatform "attiny424") ffqn fbody -- TODO: hardcoded platform
parseAVRGenerate Init =
   -- TODO: Everything is hardcoded, this should be done better
   registerCompilerFact (PlatformGeneratedFunctionSignal (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "highPA1")) (PlatformGeneratedFunction (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "highPA1") (toDyn (ByteString.pack [1, 224, 0, 147, 5, 4])))
parseAVRGenerate _ = compileOk

-- | Handle native functions. The byte code for native functions is already there, so this only checks
-- whether it exists.
transformToBytes tp@(TargetPlatform tps) ffqn NativeFunction = do
   nativeFunctionMaybe <- getCompilerFact (PlatformGeneratedFunctionSignal tp ffqn)
   case nativeFunctionMaybe of
      Just _   -> compileOk
      _        -> compilerErrorForFunction ffqn $ "Native function not found in the given target platform ("++tps++")."
-- | Handle function applicate by generating JMP code to the target function.
transformToBytes tp ffqn (FunctionExpression _) = do
   registerCompilerFact (PlatformGeneratedFunctionSignal tp ffqn) (PlatformGeneratedFunction tp ffqn $ toDyn (ByteString.pack [0, 0]))        -- TODO: dummy implementation

-- | Collect all the bytes recursively from all functions called from the supplied function
collectBytesFrom :: TargetPlatform -> FunctionFQN -> CompilerIO (Maybe ByteString.ByteString)
collectBytesFrom tp ffqn = do
   bytesMaybe   <- getCompilerFact (PlatformGeneratedFunctionSignal tp ffqn)
   fbodyMaybe   <- getCompilerFact (CompiledFunctionSignal ffqn)
   case (bytesMaybe, fbodyMaybe) of
      (Just (PlatformGeneratedFunction _ _ d), Just (CompiledFunction _ NativeFunction)) ->      -- For native functions, don't recurse
            return $ (\x -> x::ByteString.ByteString) <$> fromDynamic d
      (Just (PlatformGeneratedFunction _ _ d), Just (CompiledFunction _ fbody))          -> do   -- For non-native functions, concat this plus recursive bytes
            fbodyBytesMaybe <- recurseFunctions tp fbody
            return $ liftA2 concatByteStrings ((\x -> x::ByteString.ByteString) <$> fromDynamic d) fbodyBytesMaybe
      _                                                                                  -> return Nothing
   where
      concatByteStrings b1 b2 = ByteString.concat [b1, b2]

recurseFunctions :: TargetPlatform -> FunctionBody -> CompilerIO (Maybe ByteString.ByteString)
recurseFunctions tp (FunctionExpression (FunctionApplication calledFfqn)) = collectBytesFrom tp calledFfqn
recurseFunctions _  (FunctionExpression (NumberConstant _)) = return $ Just $ ByteString.pack [0, 0]
recurseFunctions _ NativeFunction = return Nothing -- This should not be called

