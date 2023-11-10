{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Build the Functional AST from the AST. This represents the tree of function compositions that
 - is a function's body.
 -}

module FAST (FunctionBody(..)) where

import Module

-- | A body of a function.
data FunctionBody = FunctionApplication FunctionFQN
 deriving (Eq, Show)

