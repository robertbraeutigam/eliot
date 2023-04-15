{-|
 - A generic dependency resolver engine that is configured with a set
 - of processors that work on facts. When a fact is registered,
 - all processors are asked to process that fact in the background.
 - Processors may do nothing, or wait for a set of other facts to become
 - availble, and ultimately process the fact and generate new ones in the process.
 -
 - This engine is used as the basic infrastructure of the compiler to process data
 - as it becomes available, everything from file contents to method signatures to
 - AST trees, etc.
 -
 - The engine is "finished" if there are no tasks still to be run, since only tasks
 - are able to generate new facts. And the engine is "stuck" if all of the (non-zero)
 - tasks are waiting for facts to become available.
 -}

module FactEngine(FactEngine, FactProcessor, newEngine, registerFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Data.Hashable

type FactProcessor k v = v -> FactEngine k v -> IO ()

data FactEngine k v = FactEngine {
   processors :: [FactProcessor k v],
   facts :: STMMap.Map k v,
   runningCount :: TVar Int,
   waitingCount :: TVar Int
}

-- | Create the engine with a given set of tasks.
newEngine :: [FactProcessor k v] -> IO (FactEngine k v)
newEngine ps = do
   emptyFacts <- atomically STMMap.empty
   zeroRunningCount <- atomically $ newTVar 0
   zeroWaitingCount <- atomically $ newTVar 0
   return $ FactEngine ps emptyFacts zeroRunningCount zeroWaitingCount

-- | Register a fact into the engine. This will spawn all processors
-- for this fact. Processors may choose to do nothing.
registerFact :: (Hashable k, Show k) => k -> v -> FactEngine k v -> IO () 
registerFact k v engine = do
   changed <- insertFact k v engine
   if changed then
      startProcessorsFor v engine
   else
      fail ("Fact for " ++ (show k) ++ " was generated twice, internal compiler error.")

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay there forever.
getFact :: Hashable k => k -> FactEngine k v -> IO v
getFact k engine = atomically $ do
   maybeV <- STMMap.lookup k (facts engine) 
   case maybeV of
      Just v    -> return v
      Nothing   -> retry

-- Non-exported functions:

-- | Start all processors given a fact.
startProcessorsFor :: v -> FactEngine k v -> IO ()
startProcessorsFor v engine = do
   void $ mapConcurrently startProcessor (processors engine)
   where
      startProcessor p = bracket_ incRunningCount decRunningCount (p v engine)
      incRunningCount = atomically $ modifyTVar (runningCount engine) (1+)
      decRunningCount = atomically $ modifyTVar (runningCount engine) (1-)

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => k -> v -> FactEngine k v -> IO Bool
insertFact k v engine = atomically $ do
   present <- STMMap.member k (facts engine)
   if present then pure False else (STMMap.insert k v (facts engine) >> return True)

