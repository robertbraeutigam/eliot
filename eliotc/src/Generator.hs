{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Binary code generation related types.
 -}

module Generator (TargetPlatform(..)) where

import Data.Hashable

data TargetPlatform = TargetPlatform String
   deriving (Show, Eq)

instance Hashable TargetPlatform where
  hashWithSalt salt (TargetPlatform s) = hashWithSalt salt s

