{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Handle generating machine instruction code for AVR microprocessors.
 -}

module Processor.AVR (generateAVRBinary) where

import GHC.Generics
import Data.Hashable
import CompilerProcessor
import Data.Tree
import Processor.FAST
import Processor.Module
import Data.ByteString
import Processor.AVRInstruction

data FunctionByteBlockSignal = FunctionByteBlockSignal FunctionFQN
   deriving (Eq, Generic)
instance Hashable FunctionByteBlockSignal
data FunctionByteBlock = FunctionByteBlock FunctionFQN ByteString

-- TODO: currently this ignores target platform
-- | Generate the AVR bytecode for each compiled function.
generateAVRBinary :: TargetPlatform -> SimpleCompilerProcessor CompiledFunction
generateAVRBinary (TargetPlatform targetPlatform) (CompiledFunction ffqn (NonNativeFunction expressionTree)) =
   | targetPlatform == "attiny424" = case generateBytes expressionTree of
      Right bb  -> registerCompilerFact (FunctionByteBlockSignal ffqn) (FunctionByteBlock ffqn bb)
      Left err  -> compilerErrorForFunction ffqn ("Error while generating byte code for function. " ++ err)
generateAVRBinary _                                                          = compileOk

generateAVRMain :: TargetPlatform -> FunctionFQN -> SimpleCompilerProcessor CompiledFunction
generateAVRMain tp@(TargetPlatform targetPlatform) targetFFQN@(FunctionFQN mn _) (CompiledFunction ffqn (NonNativeFunction expressionTree))
   | ffqn == targetFFQN && targetPlatform == "attiny424" = do
      bbsMaybe <- collectByteBlocks ffqn expressionTree
      case bbsMaybe of
         Just bbs -> registerCompilerFact (TargetBinaryGeneratedSignal tp) (TargetBinaryGenerated tp mn bbs)
         _        -> compileOk
   | otherwise                                           = compileOk
generateAVRMain (TargetPlatform targetPlatform) targetFFQN (CompiledFunction ffqn NativeFunction)
   | ffqn == targetFFQN && targetPlatform == "attiny424" = compilerErrorForFunction ffqn "Main can not be native."
   | otherwise                                           = compileOk

-- | Generate the bytes for a single expression tree. This is done by depth-first calling functions and leaving their
-- results on the stack. This way the stack will always contain the arguments for the next (parent) function call.
generateBytes :: Tree Expression -> Either String ByteString
generateBytes exprTree = foldTree generateBlockBytes exprTree

-- TODO: everything assumes all data is 1 byte here, so this is obviously wrong
generateBlockBytes :: Expression -> [Either String ByteString] -> Either String ByteString
generateBlockBytes e ps = mconcat <$> (sequence (ps ++ [generateSingleBlockBytes e]))

generateSingleBlockBytes :: Expression -> Either String ByteString
generateSingleBlockBytes (NumberConstant i)               = instructionsToBytes [Ldi R16 (fromIntegral i), Push R16]
generateSingleBlockBytes (FunctionApplication _)          = instructionsToBytes [Rcall 0]

-- | Collect all byte blocks recursively for a given function and its call tree.
collectByteBlocks :: FunctionFQN -> Tree Expression -> CompilerIO Maybe ByteString
collectByteBlocks ffqn te = do
   functionBlock   <- getCompilerFact (FunctionByteBlockSignal ffqn)
   recursiveBlocks <- 

