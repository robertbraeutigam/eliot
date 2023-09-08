{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Module information.
 -}

module Module (ModuleName(..)) where

import Data.Hashable

-- | A fully qualified module name.
data ModuleName = ModuleName [String] String
 deriving (Eq, Show)

instance Hashable ModuleName where
  hashWithSalt salt (ModuleName ps m) = hashWithSalt salt (ps, m)

