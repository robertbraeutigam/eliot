{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Build the Functional AST from the AST. This identifies the called functions and their parameters and checks
 - everything for type-safety.
 -}

module FAST (Module(..), FunctionApplication(..), ModuleName(..), FunctionName(..)) where

import Data.Hashable
import Data.Map

-- | Represents a full module and all defined functions.
data Module = Module ModuleName (Map String FunctionApplication)
 deriving (Eq, Show)

-- | A single function application.
data FunctionApplication = FunctionApplication FunctionName
 deriving (Eq, Show)

-- | A fully qualified module name.
data ModuleName = ModuleName [String] String
 deriving (Eq, Show)

instance Hashable ModuleName where
  hashWithSalt salt (ModuleName ps m) = hashWithSalt salt (ps, m)

-- | A fully qualified function name.
data FunctionName = FunctionName ModuleName String
 deriving (Eq, Show)

