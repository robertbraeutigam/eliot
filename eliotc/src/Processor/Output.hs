{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Writing the compiled output to the filesystem.
 -}

module Processor.Output (TargetBinaryGeneratedSignal(..), TargetBinaryGenerated(..), writeOutputBinary) where

import CompilerProcessor
import Processor.Module
import Processor.Main
import qualified Data.ByteString as ByteString
import Control.Monad.Trans

data TargetBinaryGeneratedSignal = TargetBinaryGeneratedSignal TargetPlatform

-- | The target platform produced the compiled version of the source code
data TargetBinaryGenerated = TargetBinaryGenerated TargetPlatform ModuleName ByteString.ByteString                 

writeOutputBinary :: SimpleCompilerProcessor TargetBinaryGenerated
writeOutputBinary (TargetBinaryGenerated (TargetPlatform tp) (ModuleName _ mn) bs) = do
   lift $ ByteString.writeFile (mn ++ "." ++ tp ++ ".bin") bs
   infoMsg $ "Generated output file "++(mn ++ "." ++ tp ++ ".bin")++", for architecture: "++tp
   compileOk


