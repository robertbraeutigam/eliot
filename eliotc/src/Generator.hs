{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-| Binary code generation related types.
 -}

module Generator (TargetPlatform(..)) where

data TargetPlatform = Attiny424
   deriving (Show, Eq)

