{-|
 - A fact engine that accepts completely dynamic types both as keys and as values.
 -}

module Engine.DynamicFactEngine(DynamicFactEngine, DynamicFactsIO, DynamicKey(..), DynamicValue(..), resolveFacts, getFact, registerFact, toDynKey, toDynValue, fromDynValue) where

import Data.Dynamic
import Data.Hashable
import Data.Maybe
import qualified Engine.FactEngine as FactEngine

data DynamicKey = DynamicKey Dynamic (Int -> Int) (Dynamic -> Bool)

newtype DynamicValue = DynamicValue Dynamic

type DynamicFactEngine = FactEngine.FactEngine DynamicKey DynamicValue

type DynamicFactsIO = FactEngine.FactsIO DynamicKey DynamicValue

toDynKey :: (Hashable k, Typeable k) => k -> DynamicKey
toDynKey k = DynamicKey (toDyn k) ((flip hashWithSalt) k) (\d -> False `fromMaybe` ((== k) <$> (fromDynamic d))) 

toDynValue :: (Typeable v) => v -> DynamicValue
toDynValue v = DynamicValue (toDyn v)

fromDynValue :: (Typeable v) => DynamicValue -> Maybe v
fromDynValue (DynamicValue dv) = fromDynamic dv

resolveFacts :: [FactEngine.FactProcessor DynamicKey DynamicValue] -> [(DynamicKey, DynamicValue)] -> IO (Maybe [(DynamicKey, DynamicValue)])
resolveFacts ps vs = FactEngine.resolveFacts ps vs

getFact :: (Hashable k, Typeable k, Typeable v) => k -> DynamicFactsIO (Maybe v)
getFact k = do
   maybeV <- FactEngine.getFact (toDynKey k)
   case maybeV of
      Just (DynamicValue d) -> return $ fromDynamic d
      _                     -> return Nothing

registerFact :: (Hashable k, Typeable k, Typeable v) => k -> v -> DynamicFactsIO ()
registerFact k v = FactEngine.registerFact (toDynKey k) (toDynValue v)

instance Eq DynamicKey where
   (DynamicKey _ _ eqf) == (DynamicKey d2 _ _) = eqf d2

instance Hashable DynamicKey where
   hashWithSalt salt (DynamicKey _ hf _) = hf salt

instance Show DynamicKey where
   show (DynamicKey d _ _) = show d
   
