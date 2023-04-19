{-| Processor for reading directories and files.
 -}

module Files (directoryWalker, fileReader) where

import Control.Monad.Trans.Class
import System.FilePath
import System.Directory
import Control.Monad
import Data.List (isPrefixOf, isSuffixOf)
import FactEngine
import CompilerFacts
import Logging

directoryWalker :: CompilerProcessor
directoryWalker (SourcePath path) = do
   isFile      <- lift $ doesFileExist path
   isDirectory <- lift $ doesDirectoryExist path
   when isFile       $ registerFact (SourceFileDetected path) (SourceFile path) 
   when isDirectory  $ (lift $ filter (not . isPrefixOf ".") <$> listDirectory path) >>= mapM_ ((registerFact . SourcePathDetected <*> SourcePath) . (path </>))
   when ((not isFile) && (not isDirectory)) $ errorMsg $ "Path " ++ path ++ " is neither a file nor directory"
directoryWalker _ = return ()

fileReader :: CompilerProcessor
fileReader (SourceFile path)
   | ".els" `isSuffixOf` path = (lift $ readFile path) >>= (registerFact (SourceFileRead path) . SourceFileContent)
   | otherwise                = debugMsg $ "Ignoring source file because not ending in '.els': " ++ path
fileReader _ = return ()
