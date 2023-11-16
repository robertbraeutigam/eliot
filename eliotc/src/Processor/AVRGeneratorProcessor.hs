{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVRGeneratorProcessor (parseAVRGenerate) where

import CompilerProcessor
import Module
import Generator
import qualified Data.ByteString as ByteString

parseAVRGenerate :: CompilerProcessor
parseAVRGenerate (GenerateMain tp@(TargetPlatform "attiny424") (FunctionFQN moduleName _) _) =
   registerCompilerFact TargetBinaryGeneratedSignal (TargetBinaryGenerated tp moduleName (ByteString.pack [255, 207]))
parseAVRGenerate _ = compileOk


