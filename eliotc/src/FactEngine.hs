{-# LANGUAGE FlexibleContexts #-}
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
import Control.Applicative
import Control.Monad.Extra
import Control.Exception.Lifted
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
   changed <- insertFact k v
   if changed then
      startProcessorsFor v
   else
      fail ("Fact for " ++ (show k) ++ " was generated twice, internal compiler error.")

-- | Get a fact from the engine, or wait until the fact becomes
-- available. Note: a fact can not change and will stay the same forever.
getFact :: Hashable k => k -> FactsIO k v v
getFact k = bracket_ incWaitingCount decWaitingCount $ lookupFact k
   where
      incWaitingCount  = tx $ modifyEngine waitingCount (+1)
      decWaitingCount = tx $ whenM (notM isTerminated) $ modifyEngine waitingCount (subtract 1)

data TerminateProcessor = TerminateProcessor
   deriving (Show, Eq)

instance Exception TerminateProcessor

-- | Lookup a fact in the engine. If the engine is terminated however, throw
-- an exception.
lookupFact :: Hashable k => k -> FactsIO k v v
lookupFact k = tx $ do
   terminated <- isTerminated
   if terminated then
      lift $ throwSTM TerminateProcessor
   else do
      maybeV <- ask >>= (lift . STMMap.lookup k . facts)
      case maybeV of
         Just v    -> return v
         Nothing   -> lift retry

-- | Start all processors given a fact. Note that we increment the running
-- count synchronously with the returned IO, but decrease one by one as
-- those IOs terminate.
startProcessorsFor :: v -> FactsIO k v ()
startProcessorsFor v = do
   ask >>= lift . addRunningCount
   ask >>= (\e -> lift $ mapConcurrently_ (startProcessor e) (processors e))
   where
      startProcessor e p  = bracket_ (pure ()) (decRunningCount e) ((runReaderT (p v) e) `ignoreException` TerminateProcessor)
      decRunningCount e   = atomically $ modifyTVar (runningCount e) (subtract 1)
      addRunningCount e   = atomically $ modifyTVar (runningCount e) (+ (length (processors e)))
      ignoreException a e = catchJust (\r -> if r == e then Just () else Nothing) a (\_ -> return ())

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => k -> v -> FactsIO k v Bool
insertFact k v = tx $ do
   present <- ask >>= lift . STMMap.member k . facts
   if present then pure False else ((ask >>= lift . STMMap.insert k v . facts) >> return True)

-- | Create the engine with a given set of tasks.
emptyEngine :: [FactProcessor k v] -> IO (FactEngine k v)
emptyEngine ps = do
   emptyFacts       <- atomically $ STMMap.empty
   zeroRunningCount <- atomically $ newTVar 0
   zeroWaitingCount <- atomically $ newTVar 0
   return $ FactEngine ps emptyFacts zeroRunningCount zeroWaitingCount

-- | Determine if the engine is terminated. It is terminated if the running count
-- is equal to the waiting count. I.e. all processors are waiting on something and
-- no progress can be made. This is true if there are no processors too.
isTerminated :: FactsSTM k v Bool
isTerminated = liftA2 (==) (readEngine runningCount) (readEngine waitingCount)

-- | Read from one of the TVar properties of the engine
readEngine :: (FactEngine k v -> TVar a) -> FactsSTM k v a
readEngine f = ask >>= (lift . readTVar . f)

-- | Modify one of the TVar properties of the engine
modifyEngine :: (FactEngine k v -> TVar a) -> (a -> a) -> FactsSTM k v ()
modifyEngine f g = ask >>= (lift . flip modifyTVar g . f)

-- | Run the FactsSTM into FactsIO
tx :: FactsSTM k v a -> FactsIO k v a
tx s = ask >>= (lift . atomically . (runReaderT s))

