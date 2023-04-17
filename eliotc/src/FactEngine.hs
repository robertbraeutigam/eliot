{-|
 - A generic dependency resolver engine that is configured with a set
 - of processors that work on facts. When a fact is registered,
 - all processors are asked to process that fact in the background.
 - Processors may do nothing, or wait for a set of other facts to become
 - available, and ultimately process the fact and generate new ones in the process.
 -
 - This engine is used as the basic infrastructure of the compiler to process data
 - as it becomes available, everything from file contents to method signatures to
 - AST trees, etc.
 -
 - The engine is "finished" if there are no tasks still to be run, since only tasks
 - are able to generate new facts. And the engine is "stuck" if all of the (non-zero)
 - tasks are waiting for facts to become available.
 -}

module FactEngine(FactsIO, FactProcessor, resolveFacts, registerFact, getFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Data.Hashable
import Data.Maybe.HT

data FactEngine k v = FactEngine {
   processors   :: [FactProcessor k v],
   facts        :: STMMap.Map k v,
   runningCount :: TVar Int,
   waitingCount :: TVar Int
}

-- | A program built with operations on the FactEngine
type FactsIO k v = ReaderT (FactEngine k v) IO

-- | An STM program that uses the FactEngine
type FactsSTM k v = ReaderT (FactEngine k v) STM

-- | Use this type to define processors for facts that you can register in an engine.
type FactProcessor k v = v -> FactsIO k v ()

-- | Run the engine with the given processors and initial facts and wait
-- until all the possible facts are available, or there was some error.
resolveFacts :: (Hashable k, Show k) => [FactProcessor k v] -> [(k, v)] -> IO (Maybe [(k, v)])
resolveFacts ps vs = do
   engine <- emptyEngine ps
   runReaderT (sequence_ $ map (uncurry registerFact) vs) engine
   toMaybe <$> (wasSuccessful engine) <*> STMMap.unsafeToList (facts engine)
   where
      wasSuccessful engine = (== 0) <$> (atomically $ readTVar (waitingCount engine))

-- | Register a fact into the engine. This will spawn all processors
-- for this fact. Processors may choose to do nothing.
registerFact :: (Hashable k, Show k) => k -> v -> FactsIO k v () 
registerFact k v = do
   engine <- ask
   changed <- lift $ insertFact engine k v
   if changed then
      lift $ startProcessorsFor engine v
   else
      lift $ fail ("Fact for " ++ (show k) ++ " was generated twice, internal compiler error.")

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay the same forever.
getFact :: Hashable k => k -> FactsIO k v v
getFact k = do
   engine <- ask
   lift $ bracket_ (incWaitingCount engine) (decWaitingCount engine) $ lookupFact engine k
   where
      incWaitingCount engine = atomically $ modifyTVar (waitingCount engine) (+1)
      decWaitingCount engine = atomically $ do
         terminated <- (runReaderT isTerminated engine)
         if terminated then
            pure () -- If terminated, we don't want to destroy the equilibrium state, so do nothing
         else
            modifyTVar (waitingCount engine) (subtract 1)

data TerminateProcessor = TerminateProcessor
   deriving (Show, Eq)

instance Exception TerminateProcessor

-- | Lookup a fact in the engine. If the engine is terminated however, throw
-- an exception.
lookupFact :: Hashable k => FactEngine k v -> k -> IO v
lookupFact engine k = atomically $ do
   terminated <- (runReaderT isTerminated engine)
   if terminated then
      throwSTM TerminateProcessor
   else do
      maybeV <- STMMap.lookup k (facts engine) 
      case maybeV of
         Just v    -> return v
         Nothing   -> retry

-- | Determine if the engine is terminated.
isTerminated :: FactsSTM k v Bool
isTerminated = do
   currentRunningCount <- readEngine runningCount
   currentWaitingCount <- readEngine waitingCount
   return $ currentRunningCount == currentWaitingCount

readEngine :: (FactEngine k v -> TVar a) -> FactsSTM k v a
readEngine f = ask >>= (lift . readTVar . f)

-- | Start all processors given a fact. Note that we increment the running
-- count synchronously with the returned IO, but decrease one by one as
-- those IOs terminate.
startProcessorsFor :: FactEngine k v -> v -> IO ()
startProcessorsFor engine v = void $ do
   addRunningCount
   mapConcurrently startProcessor (processors engine)
   where
      startProcessor p    = bracket_ (pure ()) decRunningCount ((runReaderT (p v) engine) `ignoreException` TerminateProcessor)
      decRunningCount     = atomically $ modifyTVar (runningCount engine) (subtract 1)
      addRunningCount     = atomically $ modifyTVar (runningCount engine) (+ (length (processors engine)))
      ignoreException a e = catchJust (\r -> if r == e then Just () else Nothing) a (\_ -> return ())

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => FactEngine k v -> k -> v -> IO Bool
insertFact engine k v = atomically $ do
   present <- STMMap.member k (facts engine)
   if present then pure False else (STMMap.insert k v (facts engine) >> return True)

-- | Create the engine with a given set of tasks.
emptyEngine :: [FactProcessor k v] -> IO (FactEngine k v)
emptyEngine ps = do
   emptyFacts       <- atomically $ STMMap.empty
   zeroRunningCount <- atomically $ newTVar 0
   zeroWaitingCount <- atomically $ newTVar 0
   return $ FactEngine ps emptyFacts zeroRunningCount zeroWaitingCount

