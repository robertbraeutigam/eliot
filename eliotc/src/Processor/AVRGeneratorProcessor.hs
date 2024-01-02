{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVRGeneratorProcessor (parseAVRGenerate) where

import CompilerProcessor
import Data.Tree
import Processor.Main

parseAVRGenerate :: CompilerProcessor
parseAVRGenerate v = case getTypedValue v of
   Just (GenerateMain (TargetPlatform "attiny424") _ expressionTree) -> debugMsg ("\n" ++ (drawTree (show <$> expressionTree))) >> compileOk
      -- TODO: The platform should come from the signal to compile, later from the optimizer, or whatever, it is hardcoded here
--   Just Init                                                         ->
      -- TODO: Everything is hardcoded, this should be done better
--      registerCompilerFact (PlatformGeneratedFunctionSignal (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "high")) (PlatformGeneratedFunction (TargetPlatform "attiny424") (FunctionFQN (ModuleName [] "On") "high") (toDyn (ByteString.pack [1, 224, 0, 147, 5, 4])))
   _                                                                 -> compileOk

