{-| Defines simple output functions for logging both internal state and logging
 - compiler errors.
 -}

module Logging (errorMsg, debugMsg, compilerErrorMsg, newLogger, Logger, LoggerIO, withLogger, runLogger, infoMsg) where

import Control.Concurrent.MVar
import System.Console.ANSI
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.IO
import Control.Monad.Extra
import Control.Exception.Lifted

data Logger = Logger {
   mutex :: MVar ()
}

newLogger :: IO Logger
newLogger = Logger <$> newEmptyMVar

type LoggerIO = ReaderT Logger IO

-- | Run any of the logging directly in the IO monad.
withLogger :: Logger -> LoggerIO () -> IO ()
withLogger logger io = runReaderT io logger

-- | Run a non-protected version of the logger that is not thread-safe.
runLogger :: LoggerIO () -> IO ()
runLogger loggerIO = do
   logger <- newLogger
   withLogger logger loggerIO

-- | A generic error not in source files, but in the compiler itself
errorMsg :: String -> LoggerIO ()
errorMsg msg = syncIO $ do
   colored stdout Vivid Red "[ ERROR ] "
   hPutStrLn stdout msg

-- | Debug message
debugMsg :: String -> LoggerIO ()
debugMsg msg = syncIO $ colored stdout Dull White ("[ DEBUG ] " ++ msg) >> hPutStrLn stdout ""

-- | Info message
infoMsg :: String -> LoggerIO ()
infoMsg msg = syncIO $ colored stdout Vivid White ("[ INFO  ] " ++ msg) >> hPutStrLn stdout ""

-- | Show a compiler error in a given file with "standard" compiler output format
-- TODO: This doesn't handle multi-line failures yet.
compilerErrorMsg :: FilePath -> String -> Int -> Int -> Int -> Int -> String -> LoggerIO ()
compilerErrorMsg filePath content fRow fCol tRow tCol msg = syncIO $ do
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
colored h intensity color text = do
   whenM (hSupportsANSI h) $ hSetSGR h [SetColor Foreground intensity color]
   hPutStr h text
   whenM (hSupportsANSI h) $ hSetSGR h [Reset]
   
syncIO :: IO () -> LoggerIO ()
syncIO l = do
   m <- mutex <$> ask
   liftIO $ bracket_ (putMVar m ()) (takeMVar m) l
   
