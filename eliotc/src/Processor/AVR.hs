{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVR (generateAVRBinary) where

import CompilerProcessor
import Data.Tree
import Processor.Main

generateAVRBinary :: SimpleCompilerProcessor GenerateMain
generateAVRBinary (GenerateMain (TargetPlatform "attiny424") _ expressionTree) = debugMsg ("\n" ++ (drawTree (show <$> expressionTree))) >> compileOk
generateAVRBinary _                                                            = compileOk

