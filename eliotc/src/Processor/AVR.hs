{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVR (generateAVRBinary) where

import CompilerProcessor
import Data.Tree
import Processor.FAST

-- TODO: currently this ignores target platform
generateAVRBinary :: SimpleCompilerProcessor CompiledFunction
generateAVRBinary (CompiledFunction ffqn (NonNativeFunction expressionTree)) = debugMsg ((show ffqn) ++ ":\n" ++ (drawTree (show <$> expressionTree))) >> compileOk
generateAVRBinary (CompiledFunction ffqn NativeFunction)                     = debugMsg ((show ffqn) ++ ": is native") >> compileOk
generateAVRBinary _                                                            = compileOk

