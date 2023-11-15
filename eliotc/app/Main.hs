{-|
 - The command line program to start the compiler process for a given
 - set of source paths.
 -}
module Main (main) where

import System.Console.CmdArgs.Explicit
import Compiler.Compiler
import Data.List.Split
import Module
import Generator

data CommandLineParameters = CommandLineParameters {
   help         :: Bool,
   sourceDirs   :: [String],
   architecture :: TargetPlatform,
   mainModule   :: ModuleName
} deriving Show

-- | Start the compiler by parsing arguments and calling the
-- compile function with the appropriate parameters.
main :: IO ()
main = do
   args <- processArgs arguments
   if help args then
      print $ helpText [] HelpFormatDefault arguments
   else
      compile (mainModule args) (architecture args) (sourceDirs args)

-- | Define the command line arguments the compile can accept.
arguments :: Mode CommandLineParameters
arguments = mode "eliotc" (CommandLineParameters False [] (TargetPlatform "") (ModuleName [] "")) "The ELIOT Programming Language compiler."
   (flagArg addPath "[SOURCEDIR]...")
   [flagReq ["arch", "a"] setArch "ARCH" "The target architecture to compile to",
    flagReq ["main", "m"] setMain "MODULE" "The module name the main function to compile is in",
    flagHelpSimple setHelp]
   where
      addPath pathCandidate commandLineParameters = Right $ commandLineParameters { sourceDirs = pathCandidate: sourceDirs commandLineParameters }
      setHelp commandLineParameters = commandLineParameters { help = True }
      setArch arch commandLineParameters = Right $ commandLineParameters { architecture = TargetPlatform arch }
      setMain main commandLineParameters = case moduleName (splitOn "." main) of
         Just mn   -> Right $ commandLineParameters { mainModule = mn }
         Nothing   -> Left "Specified main module name is not a valid module name."
      moduleName [] = Nothing
      moduleName xs = Just $ ModuleName (init xs) (last xs)

