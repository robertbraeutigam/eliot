{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Build the Functional AST from the AST. This represents the tree of function compositions that
 - is a function's body.
 -}

module FAST (FunctionBody(..), Expression(..)) where

import Module
import Data.Tree

-- | A body of a function.
data FunctionBody = NativeFunction | NonNativeFunction (Tree Expression)
   deriving (Eq, Show)

-- | An expression
data Expression = NumberConstant Integer
                | FunctionApplication FunctionFQN
   deriving (Eq)

instance Show Expression where
   show (NumberConstant i) = show i
   show (FunctionApplication ffqn) = (show ffqn) ++ "()"

