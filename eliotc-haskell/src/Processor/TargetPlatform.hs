{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Ask the system to generate platform specific code for main.
 -}

module Processor.TargetPlatform (TargetPlatform(..)) where

import GHC.Generics
import Data.Hashable

data TargetPlatform = TargetPlatform String
   deriving (Show, Eq, Generic)

instance Hashable TargetPlatform

