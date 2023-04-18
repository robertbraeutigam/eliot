{-| Defines simple output functions for logging both internal state and logging
 - compiler errors.
 -}

module Logging (errorMsg) where

import System.Console.ANSI
import System.IO
import Control.Monad.Extra

-- | A generic error not in source files, but in the compiler itself
errorMsg :: String -> IO ()
errorMsg msg = do
   colored stderr Vivid Red "[ ERROR ] "
   hPutStrLn stderr msg
   
colored :: Handle -> ColorIntensity -> Color -> String -> IO ()
colored handle intensity color text = do
   whenM (hSupportsANSI handle) $ hSetSGR handle [SetColor Foreground intensity color]
   hPutStr handle text
   whenM (hSupportsANSI handle) $ hSetSGR handle [Reset]
   
