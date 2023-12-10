module Processor.FileProcessors (directoryWalker, fileReader) where

import CompilerProcessor
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans

directoryWalker :: CompilerProcessor
directoryWalker v = case getTypedValue v of
   Just (SourcePath path) -> do
      isFile      <- lift $ doesFileExist path
      isDirectory <- lift $ doesDirectoryExist path
      when isFile       $ registerCompilerFact (SourceFileSignal path) (SourceFile path) 
      when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerCompilerFact . SourcePathSignal <*> SourcePath) . (path </>))
      when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
      compileOk
   _                      -> compileOk

fileReader :: CompilerProcessor
fileReader v = case getTypedValue v of
   Just (SourcePath path) | ".els" `isSuffixOf` path -> (lift $ readFile path) >>= (registerCompilerFact (SourceFileContentSignal path) . (SourceFileContent path)) >> compileOk
   Just (SourcePath path) | otherwise                -> (debugMsg $ "Ignoring source file because not ending in '.els': " ++ path) >> compileOk
   _                                                 -> compileOk


