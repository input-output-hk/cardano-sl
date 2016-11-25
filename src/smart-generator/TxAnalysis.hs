{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module TxAnalysis
       ( createTxTimestamps
       , registerSentTx
       , checkWorker
       ) where

import           Control.Lens           ((^.))
import           Control.TimeWarp.Timed (repeatForever)
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (intersect)
import           Data.Maybe             (fromJust)
import           Data.Text.IO           (appendFile)
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

import           Util                   (verifyCsvFile, verifyCsvFormat)

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

checkTxsInLastBlock :: forall ssc . SscConstraint ssc
                    => TxTimestamps -> ProductionMode ssc ()
checkTxsInLastBlock TxTimestamps {..} = do
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

                -- Delete verified txs from hashmap
                let newSt = foldr M.delete st verified
                liftIO $ writeIORef sentTimes newSt

                -- We don't know exact time when checked block has been created/adopted,
                -- but we do know that it was not at `k` depth a slot ago,
                -- so we just take a beginning of current slot
                slStart <- getSlotStart =<< getCurrentSlot
                liftIO $ writeIORef lastSlot curSlot

                let verifiedSentTimes = map (fromJust . flip M.lookup st) verified
                    df = zip3 verified verifiedSentTimes $ repeat (fromIntegral slStart)
                    dfText = mconcat $ map verifyCsvFormat df

                liftIO $ appendFile verifyCsvFile dfText

checkWorker :: forall ssc . SscConstraint ssc
            => TxTimestamps -> ProductionMode ssc ()
checkWorker txts = repeatForever slotDuration onError $
                   checkTxsInLastBlock txts
  where onError e = slotDuration <$
                    logWarning (sformat ("Error occured in checkWorker: " %build) e)

