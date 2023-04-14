{-|
 - A generic dependency resolver engine that is configured with a set
 - of processors that work on facts. When a fact is registered,
 - all processors are asked to process that fact in the background.
 - Processors may do nothing, or wait for a set of other facts to become
 - availble, and ultimately process the fact and may generate new ones in the process.
 -}

module Engine(Engine, FactProcessor, newEngine, registerFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.STM
import Data.Hashable

type FactProcessor v = v -> IO ()

data Engine k v = Engine {
   processors :: [FactProcessor v],
   facts :: STMMap.Map k v
}

-- | Create the engine with a given set of tasks.
newEngine :: [FactProcessor v] -> IO (Engine k v)
newEngine ps = do
   emptyFacts <- atomically STMMap.empty
   return $ Engine ps emptyFacts

-- | Register a fact into the engine. This will spawn all processors
-- for this fact. Processors may choose to do nothing.
registerFact :: Hashable k => k -> v -> Engine k v -> IO () 
registerFact k v engine = atomically $ do
   present <- STMMap.member k (facts engine)
   if present then pure () else STMMap.insert k v (facts engine)

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay there forever.
getFact :: Hashable k => k -> Engine k v -> IO v
getFact k engine = atomically $ do
   maybeV <- STMMap.lookup k (facts engine) 
   case maybeV of
      Just v    -> return v
      Nothing   -> retry

