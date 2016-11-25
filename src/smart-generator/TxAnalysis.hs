{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module TxAnalysis
       ( createTxTimestamps
       , registerSentTx
       , registerVerifiedTx
       , checkWorker
       , dumpTxTable
       ) where

import           Control.Lens           ((^.))
import           Control.TimeWarp.Timed (repeatForever)
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (intersect)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (logWarning)
import           Universum

import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (hash)
import           Pos.Slotting           (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.State              (getBlockByDepth)
import           Pos.Types              (SlotId (..), TxId, blockSlot, blockTxs)
import           Pos.WorkMode           (ProductionMode)


type TxTimeMap = M.HashMap TxId Word64

data TxTimestamps = TxTimestamps
    { sentTimes   :: IORef TxTimeMap
    , verifyTimes :: IORef TxTimeMap
    , lastSlot    :: IORef SlotId
    }

createTxTimestamps :: IO TxTimestamps
createTxTimestamps = TxTimestamps
                     <$> newIORef M.empty
                     <*> newIORef M.empty
                     <*> newIORef (SlotId 0 0)

registerSentTx :: TxTimestamps -> TxId -> Word64 -> IO ()
registerSentTx TxTimestamps{..} id = modifyIORef' sentTimes . M.insert id

registerVerifiedTx :: TxTimestamps -> TxId -> Word64 -> IO ()
registerVerifiedTx TxTimestamps{..} id = modifyIORef' verifyTimes . M.insert id

dumpTxTable :: TxTimestamps -> IO [(TxId, Word64, Word64)]
dumpTxTable TxTimestamps {..} = M.foldlWithKey' foo []
                                <$> (M.intersectionWith (,)
                                     <$> readIORef sentTimes
                                     <*> readIORef verifyTimes)
  where foo ls id (sent, verified) = (id, sent, verified) : ls

checkTxsInLastBlock :: forall ssc . SscConstraint ssc
                    => TxTimestamps -> ProductionMode ssc ()
checkTxsInLastBlock txts@TxTimestamps {..} = do
    mBlock <- getBlockByDepth k
    case mBlock of
        Nothing -> pure ()
        Just (Left _) -> pure ()
        Just (Right block) -> do
            st <- liftIO $ readIORef sentTimes
            vt <- liftIO $ readIORef verifyTimes
            ls <- liftIO $ readIORef lastSlot
            let curSlot = block^.blockSlot
            when (ls < curSlot) $ do
                let toCheck = M.keys $ M.difference st vt
                    txsMerkle = block^.blockTxs
                    txIds = map hash $ toList txsMerkle
                    verified = toCheck `intersect` txIds
                -- We don't know exact time when checked block has been created/adopted,
                -- but we do know that it was not at `k` depth a slot ago,
                -- so we just take a beginning of current slot
                slStart <- getSlotStart =<< getCurrentSlot
                forM_ verified $ \id ->
                    liftIO $ registerVerifiedTx txts id $ fromIntegral slStart
                liftIO $ writeIORef lastSlot curSlot

checkWorker :: forall ssc . SscConstraint ssc
            => TxTimestamps -> ProductionMode ssc ()
checkWorker txts = repeatForever slotDuration onError $
                   checkTxsInLastBlock txts
  where onError e = slotDuration <$
                    logWarning (sformat ("Error occured in checkWorker: " %build) e)

