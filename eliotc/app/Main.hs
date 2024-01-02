{-|
 - The command line program to start the compiler process for a given
 - set of source paths.
 -}
module Main (main) where

import System.Console.CmdArgs.Explicit
import Compiler.Compiler
import Data.List.Split
import System.IO
import Processor.Main
import Module

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
   case args of
      CommandLineParameters True _ _ _                -> printHelp
      CommandLineParameters _ _ (TargetPlatform "") _ -> hPutStrLn stderr "Target platform not specified." >> printHelp
      CommandLineParameters _ _ _ (ModuleName [] "")  -> hPutStrLn stderr "Main module name not specified." >> printHelp
      CommandLineParameters _ ss arch mn              -> compile mn arch ss
   where
      printHelp = print $ helpText [] HelpFormatDefault arguments

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
      setMain m commandLineParameters = case moduleName (splitOn "." m) of
         Just mn   -> Right $ commandLineParameters { mainModule = mn }
         Nothing   -> Left "Specified main module name is not a valid module name."
      moduleName [] = Nothing
      moduleName xs = Just $ ModuleName (init xs) (last xs)

