{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Writing the compiled output to the filesystem.
 -}

module Processor.Output (TargetBinaryGeneratedSignal(..), TargetBinaryGenerated(..), writeOutputBinary) where

import CompilerProcessor
import Module
import Generator
import qualified Data.ByteString as ByteString
import Control.Monad.Trans

data TargetBinaryGeneratedSignal = TargetBinaryGeneratedSignal TargetPlatform

-- | The target platform produced the compiled version of the source code
data TargetBinaryGenerated = TargetBinaryGenerated TargetPlatform ModuleName ByteString.ByteString                 

writeOutputBinary :: CompilerProcessor
writeOutputBinary v = case getTypedValue v of
   Just (TargetBinaryGenerated (TargetPlatform tp) (ModuleName _ mn) bs) ->
      (lift $ ByteString.writeFile (mn ++ "." ++ tp ++ ".bin") bs) >> (infoMsg $ "Generated output file "++(mn ++ "." ++ tp ++ ".bin")++", for architecture: "++tp) >> compileOk
   _                                                                     -> compileOk


