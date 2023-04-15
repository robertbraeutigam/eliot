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

module FactEngine(FactProcessor, resolveFacts, registerFact, getFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Data.Hashable

-- | Use this type to define processors for facts that you can register in an engine.
type FactProcessor k v = FactEngine k v -> v -> IO ()

data FactEngine k v = FactEngine {
   processors :: [FactProcessor k v],
   facts :: STMMap.Map k v,
   runningCount :: TVar Int,
   waitingCount :: TVar Int
}

-- | Run the engine with the given processors and initial facts and wait
-- until all the possible facts are available, or there was some error.
resolveFacts :: Hashable k => [FactProcessor k v] -> [(k, v)] -> IO (Maybe [(k, v)])
resolveFacts ps vs = do
   engine <- newEngine ps vs
   awaitTermination engine
   endRunningCount <- atomically $ readTVar (runningCount engine)
   if endRunningCount == 0 then
      fmap Just $ STMMap.unsafeToList (facts engine)
   else
      return Nothing 

-- | Register a fact into the engine. This will spawn all processors
-- for this fact. Processors may choose to do nothing.
registerFact :: (Hashable k, Show k) => FactEngine k v -> k -> v -> IO () 
registerFact engine k v = do
   changed <- insertFact engine k v
   if changed then
      startProcessorsFor engine v
   else
      fail ("Fact for " ++ (show k) ++ " was generated twice, internal compiler error.")

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay the same forever.
getFact :: Hashable k => FactEngine k v -> k -> IO v
getFact engine k =
   bracket_ incWaitingCount decWaitingCount lookupFact
   where
      incWaitingCount = atomically $ modifyTVar (waitingCount engine) (1+)
      decWaitingCount = atomically $ modifyTVar (waitingCount engine) (1-)
      lookupFact = atomically $ do
         maybeV <- STMMap.lookup k (facts engine) 
         case maybeV of
            Just v    -> return v
            Nothing   -> retry

-- Non-exported functions:

-- | Start all processors given a fact.
startProcessorsFor :: FactEngine k v -> v -> IO ()
startProcessorsFor engine v = do
   void $ mapConcurrently startProcessor (processors engine)
   where
      startProcessor p = bracket_ incRunningCount decRunningCount (p engine v)
      incRunningCount = atomically $ modifyTVar (runningCount engine) (1+)
      decRunningCount = atomically $ modifyTVar (runningCount engine) (1-)

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => FactEngine k v -> k -> v -> IO Bool
insertFact engine k v = atomically $ do
   present <- STMMap.member k (facts engine)
   if present then pure False else (STMMap.insert k v (facts engine) >> return True)

-- | Create the engine with a given set of tasks.
newEngine :: Hashable k => [FactProcessor k v] -> [(k, v)] -> IO (FactEngine k v)
newEngine ps vs = do
   emptyFacts       <- STMMap.fromList vs
   zeroRunningCount <- atomically $ newTVar 0
   zeroWaitingCount <- atomically $ newTVar 0
   return $ FactEngine ps emptyFacts zeroRunningCount zeroWaitingCount

-- | Wait for the termination of the engine. This is either that that there
-- are no more running processors, or that all processors are blocked.
awaitTermination :: FactEngine k v -> IO ()
awaitTermination engine = atomically $ do
   currentRunningCount <- readTVar $ runningCount engine
   currentWaitingCount <- readTVar $ waitingCount engine
   if currentRunningCount == currentWaitingCount then
      return ()
   else
      retry

