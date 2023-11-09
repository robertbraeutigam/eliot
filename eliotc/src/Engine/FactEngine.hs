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

module Engine.FactEngine(FactEngine, FactsIO, FactProcessor, resolveFacts, registerFact, getFact) where

import qualified Control.Concurrent.STM.Map as STMMap
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.STM
import Control.Monad.Extra
import Control.Exception.Lifted
import Control.Concurrent.Async.Lifted
import Data.Hashable
import Data.Maybe.HT
import Engine.FactEngineStatus

data FactEngine k v = FactEngine {
   processors   :: [FactProcessor k v],
   facts        :: STMMap.Map k v,
   status       :: FactEngineStatus
}

-- | A program built with operations on the FactEngine
type FactsIO k v = ReaderT (FactEngine k v) IO

-- | Use this type to define processors for facts that you can register in an engine.
type FactProcessor k v = v -> FactsIO k v ()

-- | Run the engine with the given processors and initial facts and wait
-- until all the possible facts are available, or there was some error.
resolveFacts :: (Hashable k, Show k) => [FactProcessor k v] -> [(k, v)] -> IO (Maybe [(k, v)])
resolveFacts ps vs = do
   engine      <- emptyEngine ps
   runReaderT (sequence_ $ map (uncurry registerFact) vs) engine
   termination <- runReaderT (txStatus waitForTermination) engine
   (toMaybe (termination == Success)) <$> (STMMap.unsafeToList (facts engine))

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
getFact :: (Hashable k) => k -> FactsIO k v v
getFact k = bracket_ (txStatus incWaitingCount) (txStatus decWaitingCount) $ lookupFact k

data TerminateProcessor = TerminateProcessor
   deriving (Show, Eq)

instance Exception TerminateProcessor

-- | Lookup a fact in the engine. If the engine is stalled however, throw
-- an exception.
lookupFact :: Hashable k => k -> FactsIO k v v
lookupFact k = tx $ do
   maybeV  <- (facts <$> ask) >>= (lift . STMMap.lookup k)
   stalled <- liftStatus isStalled
   case maybeV of
      Just v            -> return v
      Nothing | stalled -> lift $ throwSTM TerminateProcessor
      _                 -> lift retry

-- | Start all processors given a fact. Note that we increment the running
-- count synchronously with the returned IO, but decrease one by one as
-- those IOs terminate. TODO: this swallows potential exceptions!
startProcessorsFor :: v -> FactsIO k v ()
startProcessorsFor v = do
   engine <- ask
   txStatus $ addRunningCount (length (processors engine))
   void $ async $ mapConcurrently_ startProcessor (processors engine)
   where
      startProcessor p    = bracket_ (pure ()) (txStatus decRunningCount) ((p v) `catch` crash)
      crash :: SomeException -> FactsIO k v ()
      crash _ = txStatus incCrashedCount

-- | Insert the fact into the engine and return whether it was inserted.
insertFact :: Hashable k => k -> v -> FactsIO k v Bool
insertFact k v = tx $ withReaderT facts $ do
   present <- ask >>= lift . STMMap.member k
   if present then pure False else ((ask >>= lift . STMMap.insert k v) >> return True)

-- | Create the engine with a given set of tasks.
emptyEngine :: [FactProcessor k v] -> IO (FactEngine k v)
emptyEngine ps = do
   emptyFacts       <- atomically $ STMMap.empty
   emptyStatus      <- atomically $ initialStatus
   return $ FactEngine ps emptyFacts emptyStatus

liftStatus :: StatusSTM a -> ReaderT (FactEngine k v) STM a
liftStatus = withReaderT status

txStatus :: StatusSTM a -> FactsIO k v a
txStatus = tx . liftStatus

tx :: ReaderT (FactEngine k v) STM a -> FactsIO k v a
tx ea = ask >>= lift . atomically . (runReaderT ea)

