{-|
 - Indicates what status the fact engine is currently.
 -}

module Engine.FactEngineStatus(FactEngineTermination (..), FactEngineStatus, StatusSTM, initialStatus, isTerminated, isStalled, waitForTermination, incWaitingCount, decWaitingCount, addRunningCount, decRunningCount, incCrashedCount, printStatus) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Extra

data FactEngineStatus = FactEngineStatus {
   runningCount :: TVar Int,
   waitingCount :: TVar Int,
   crashedCount :: TVar Int
}

data FactEngineTermination = Success | Error
   deriving Eq

type StatusSTM = ReaderT FactEngineStatus STM

-- | Create an initial status with all counters as zero.
initialStatus :: STM FactEngineStatus
initialStatus = do
   zeroRunningCount <- newTVar 0
   zeroWaitingCount <- newTVar 0
   zeroCrashedCount <- newTVar 0
   return $ FactEngineStatus zeroRunningCount zeroWaitingCount zeroCrashedCount

-- Increment / Decrement various counters.
incWaitingCount :: StatusSTM ()
incWaitingCount = modifyStatus waitingCount (+1)

decWaitingCount :: StatusSTM ()
decWaitingCount = modifyStatus waitingCount (subtract 1)

addRunningCount :: Int -> StatusSTM ()
addRunningCount c = modifyStatus runningCount (+c)

decRunningCount :: StatusSTM ()
decRunningCount = modifyStatus runningCount (subtract 1)

incCrashedCount :: StatusSTM ()
incCrashedCount = modifyStatus crashedCount (+1)

-- | Determine if the engine is terminated. It is terminated if the running
-- count is zero. Note that all processors either crash or eventually exit.
isTerminated :: StatusSTM Bool
isTerminated = (== 0) <$> (readStatus runningCount)

-- | Wait for the engine to terminate. That is, until the running count is zero.
waitForTermination :: StatusSTM FactEngineTermination
waitForTermination = ifM isTerminated
   (ifM ((== 0) <$> (readStatus crashedCount)) (return Success) (return Error))
   (lift retry)

-- | Determine if the engine is stalled. The engine is stalled if the running
-- count is equal to the waiting count. This means there's a deadlock, or some
-- fact or facts are not available.
isStalled :: StatusSTM Bool
isStalled = liftM2 (==) (readStatus runningCount) (readStatus waitingCount)

-- | Read from one of the TVar properties of the status
readStatus :: (FactEngineStatus -> TVar a) -> StatusSTM a
readStatus f = ask >>= (lift . readTVar . f)

-- | Modify one of the TVar properties of the status
modifyStatus :: (FactEngineStatus -> TVar a) -> (a -> a) -> StatusSTM ()
modifyStatus f g = ask >>= (lift . flip modifyTVar g . f)

-- | Create a string representation of the status structure.
printStatus :: StatusSTM String
printStatus = do
   rc <- readStatus runningCount
   wc <- readStatus waitingCount
   cc <- readStatus crashedCount
   return $ "Status running: " ++ (show rc) ++ ", waiting: " ++ (show wc) ++ ", crashed: " ++ (show cc)

   
