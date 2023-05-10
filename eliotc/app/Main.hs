{-|
 - The command line program to start the compiler process for a given
 - set of source paths.
 -}
module Main (main) where

import System.Console.CmdArgs.Explicit
import Compiler.Compiler

data CommandLineParameters = CommandLineParameters { help :: Bool, sourceDirs :: [String] }
   deriving Show

-- | Start the compiler by parsing arguments and calling the
-- compile function with the appropriate parameters.
main :: IO ()
main = do
   args <- processArgs arguments
   if help args then
      print $ helpText [] HelpFormatDefault arguments
   else
      compile $ sourceDirs args

-- | Define the command line arguments the compile can accept.
arguments :: Mode CommandLineParameters
arguments = mode "eliotc" (CommandLineParameters False []) "The ELIOT Programming Language compiler."
   (flagArg addPath "[SOURCEDIR]...")
   [flagHelpSimple setHelp]
   where
      addPath pathCandidate commandLineParameters = Right $ commandLineParameters { sourceDirs = pathCandidate: sourceDirs commandLineParameters }
      setHelp commandLineParameters = commandLineParameters { help = True }
