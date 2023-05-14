module Processor.FileProcessors (directoryWalker, fileReader) where

import CompilerProcessor
import Logging
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans

directoryWalker :: CompilerProcessor
directoryWalker (SourcePath path) = do
   isFile      <- lift $ doesFileExist path
   isDirectory <- lift $ doesDirectoryExist path
   when isFile       $ registerCompilerFact (SourceFileDetected path) (SourceFile path) 
   when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerCompilerFact . SourcePathDetected <*> SourcePath) . (path </>))
   when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
   compileOk
directoryWalker _ = compileOk

fileReader :: CompilerProcessor
fileReader (SourceFile path)
   | ".els" `isSuffixOf` path = (lift $ readFile path) >>= (registerCompilerFact (SourceFileRead path) . (SourceFileContent path)) >> compileOk
   | otherwise                = (debugMsg $ "Ignoring source file because not ending in '.els': " ++ path) >> compileOk
fileReader _ = compileOk

