module Main (main) where

import System.Console.CmdArgs.Explicit

-- | Start the compiler by parsing arguments and calling the
-- compile function with the appropriate parameters.
main :: IO ()
main = do
   args <- processArgs arguments
   if ("help", "") `elem` args then
      print $ helpText [] HelpFormatDefault arguments
   else
      print args

arguments :: Mode [(String, String)]
arguments = mode "eliotc" [] "The ELIOT Programming Language compiler."
   (flagArg (save "source") "[SOURCEDIR]...")
   [flagHelpSimple (("help", ""):)]
   where save key value acc = Right $ (key, value):acc
