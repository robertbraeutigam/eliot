{-| Defines simple output functions for logging both internal state and logging
 - compiler errors.
 -}

module Logging (errorMsg, debugMsg, compilerErrorMsg) where

import System.Console.ANSI
import Control.Monad.IO.Class
import System.IO
import Control.Monad.Extra

-- | A generic error not in source files, but in the compiler itself
errorMsg :: MonadIO m => String -> m ()
errorMsg msg = liftIO $ do
   colored stdout Vivid Red "[ ERROR ] "
   hPutStrLn stdout msg

-- | Debug message
debugMsg :: MonadIO m => String -> m ()
debugMsg msg = liftIO $ colored stdout Dull White ("[ DEBUG ] " ++ msg) >> hPutStrLn stdout ""

-- | Show a compiler error in a given file with "standard" compiler output format
-- TODO: This doesn't handle multi-line failures yet.
compilerErrorMsg :: MonadIO m => FilePath -> String -> Int -> Int -> Int -> Int -> String -> m ()
compilerErrorMsg filePath content fRow fCol tRow tCol msg = liftIO $ do
   colored stderr Vivid White (filePath ++ ":")
   colored stderr Vivid Red "error"
   hPutStrLn stderr (":"++(show fRow)++":"++(show fCol)++":"++msg)
   colored stderr Vivid Magenta (markerSpace ++ " | \n")
   colored stderr Vivid Magenta (lineMarker ++ " | ")
   hPutStr stderr (take (fCol-1) ((lines content) !! (fRow-1)))
   colored stderr Vivid Red (take (tCol-fCol) (drop (fCol-1) ((lines content) !! (fRow-1))))
   hPutStrLn stderr (drop (tCol-1) ((lines content) !! (fRow-1)))
   colored stderr Vivid Magenta (markerSpace ++ " | ")
   colored stderr Vivid Red $ (replicate (fCol-1) ' ') ++ (replicate (tCol-fCol) '^') ++ "\n"
   where
      lineMarker = show fRow
      markerSpace = replicate (length lineMarker) ' '

colored :: Handle -> ColorIntensity -> Color -> String -> IO ()
colored handle intensity color text = do
   whenM (hSupportsANSI handle) $ hSetSGR handle [SetColor Foreground intensity color]
   hPutStr handle text
   whenM (hSupportsANSI handle) $ hSetSGR handle [Reset]
   
