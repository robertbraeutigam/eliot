module Processor.FileProcessors (directoryWalker, fileReader, initPaths) where

import CompilerProcessor
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans

-- | Receives a file path. If the path is a file, this will create a `SourceFile`. If the path refers to a directory,
-- this will recurse and create new `SourcePath` signals for those.
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

-- | Receives source files. This reads the source file and creates `SourceFileContent` signals.
fileReader :: CompilerProcessor
fileReader v = case getTypedValue v of
   Just (SourceFile path) | ".els" `isSuffixOf` path -> (lift $ readFile path) >>= (registerCompilerFact (SourceFileContentSignal path) . (SourceFileContent path)) >> compileOk
   Just (SourceFile path) | otherwise                -> (debugMsg $ "Ignoring source file because not ending in '.els': " ++ path) >> compileOk
   _                                                 -> compileOk

-- | Initializes all the paths with the given initial path arguments from the compiler arguments.
initPaths :: [String] -> CompilerProcessor
initPaths paths v = case getTypedValue v of
   Just Init -> mapM_ (\s -> registerCompilerFact (SourcePathSignal s) (SourcePath s)) paths
   _         -> compileOk

