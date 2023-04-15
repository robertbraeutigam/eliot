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

module FactEngine(FactEngine, FactProcessor, ProcessorEngine, newEngine, registerFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Data.Hashable

type FactProcessor k v = v -> ProcessorEngine k v -> IO ()

data FactEngine k v = FactEngine {
   processors :: [FactProcessor k v],
   facts :: STMMap.Map k v,
   nextId :: TVar Int, -- The next id to be used for a running task
   running :: STMMap.Map Int Int -- Id and the number of Waits that processor has
}

data ProcessorEngine k v = ProcessorEngine {
   engine :: FactEngine k v,
   id :: Int -- The id of this running processor
}

-- | Create the engine with a given set of tasks.
newEngine :: [FactProcessor k v] -> IO (FactEngine k v)
newEngine ps = do
   emptyFacts <- atomically STMMap.empty
   firstId <- atomically $ newTVar 1
   emptyRunning <- atomically STMMap.empty
   return $ FactEngine ps emptyFacts firstId emptyRunning

-- | Register a fact into the engine. This will spawn all processors
-- for this fact. Processors may choose to do nothing.
registerFact :: (Hashable k, Show k) => k -> v -> FactEngine k v -> IO () 
registerFact k v engine = do
   changed <- insertFact k v engine
   if changed then
      startProcessorsFor v engine
   else
      fail ("Fact for " ++ (show k) ++ " was generated twice, internal compiler error.")

-- | Start all processors given a fact.
startProcessorsFor :: v -> FactEngine k v -> IO ()
startProcessorsFor v engine = do
   firstId <- atomically $ readTVar (nextId engine) >>= (\id -> writeTVar (nextId engine) (id+(length (processors engine))) >> return id) 
   let ps = zip [firstId..(firstId+(length (processors engine)))] (processors engine)
   mapConcurrently (\(id, processor) -> processor v (ProcessorEngine engine id)) ps
   return ()
-- TODO: register running processors

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => k -> v -> FactEngine k v -> IO Bool
insertFact k v engine = atomically $ do
   present <- STMMap.member k (facts engine)
   if present then pure False else (STMMap.insert k v (facts engine) >> return True)

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay there forever.
getFact :: Hashable k => k -> FactEngine k v -> IO v
getFact k engine = atomically $ do
   maybeV <- STMMap.lookup k (facts engine) 
   case maybeV of
      Just v    -> return v
      Nothing   -> retry

