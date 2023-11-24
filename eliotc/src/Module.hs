{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-| Module information.
 -}

module Module (ModuleName(..), FunctionFQN(..), FunctionDictionary) where

import Data.Hashable
import Data.Map
import Data.List(intercalate)

-- | A fully qualified module name.
data ModuleName = ModuleName [String] String
   deriving (Eq)

instance Show ModuleName where
   show (ModuleName moduleNames name) = intercalate "." (moduleNames ++ [name])

instance Hashable ModuleName where
   hashWithSalt salt (ModuleName ps m) = hashWithSalt salt (ps, m)

-- | Fully qualified function name.
data FunctionFQN = FunctionFQN ModuleName String
   deriving (Eq)

instance Show FunctionFQN where
   show (FunctionFQN mn fn) = (show mn) ++ "." ++ fn

instance Hashable FunctionFQN where
   hashWithSalt salt (FunctionFQN mn n) = hashWithSalt salt (mn, n)

type FunctionDictionary = Map String FunctionFQN
