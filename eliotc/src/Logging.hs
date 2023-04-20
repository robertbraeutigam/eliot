{-| Defines simple output functions for logging both internal state and logging
 - compiler errors.
 -}

module Logging (errorMsg, debugMsg) where

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
   
colored :: Handle -> ColorIntensity -> Color -> String -> IO ()
colored handle intensity color text = do
   whenM (hSupportsANSI handle) $ hSetSGR handle [SetColor Foreground intensity color]
   hPutStr handle text
   whenM (hSupportsANSI handle) $ hSetSGR handle [Reset]
   