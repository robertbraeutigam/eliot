{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Writing the compiled output to the filesystem.
 -}

module Processor.BinaryOutputProcessor (writeOutputBinary) where

import CompilerProcessor
import Module
import Generator
import qualified Data.ByteString as ByteString
import Control.Monad.Trans

writeOutputBinary :: CompilerProcessor
writeOutputBinary Init = do
   generatedBinary <- getCompilerFact TargetBinaryGeneratedSignal
   case generatedBinary of
      Just (TargetBinaryGenerated (TargetPlatform tp) (ModuleName _ mn) bs) -> (lift $ ByteString.writeFile (mn ++ "." ++ tp ++ ".bin") bs) >> (infoMsg $ "Generated output file "++(mn ++ "." ++ tp ++ ".bin")++", for architecture: "++tp) >> compileOk
      _                                  -> errorMsg "No output generated because of previous errors, not valid target platform."
writeOutputBinary _ = compileOk


