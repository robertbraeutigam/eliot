{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVRGeneratorProcessor (parseAVRGenerate) where

import CompilerProcessor
import Module
import Data.Tree
import Generator
import Data.Dynamic
import qualified Data.ByteString as ByteString

parseAVRGenerate :: CompilerProcessor
parseAVRGenerate (GenerateMain (TargetPlatform "attiny424") _ expressionTree) = debugMsg ("\n" ++ (drawTree (show <$> expressionTree))) >> compileOk
-- TODO: The platform should come from the signal to compile, later from the optimizer, or whatever, it is hardcoded here
parseAVRGenerate Init =
   -- TODO: Everything is hardcoded, this should be done better
   registerCompilerFact (PlatformGeneratedFunctionSignal (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "high")) (PlatformGeneratedFunction (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "high") (toDyn (ByteString.pack [1, 224, 0, 147, 5, 4])))
parseAVRGenerate _ = compileOk

