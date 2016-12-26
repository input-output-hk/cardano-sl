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
import           Data.List              (intersect, last)
import           Data.Maybe             (fromJust, maybeToList)
import           Formatting             (build, sformat, (%))
import           System.FilePath.Posix  ((</>))
import           System.Wlog            (logWarning)
import           Universum

import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (hash)
import           Pos.DB                 (loadBlocksFromTipWhile)
import           Pos.Slotting           (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.Types              (SlotId (..), TxId, blockSlot, blockTxs)
import           Pos.WorkMode           (ProductionMode)

import           Util                   (verifyCsvFile, verifyCsvFormat)

type TxTimeMap = M.HashMap TxId (Int, Word64)

data TxTimestamps = TxTimestamps
    { sentTimes :: IORef TxTimeMap
    , lastSlot  :: IORef SlotId
    }

createTxTimestamps :: IO TxTimestamps
createTxTimestamps = TxTimestamps
                     <$> newIORef M.empty
                     <*> newIORef (SlotId 0 0)

registerSentTx :: TxTimestamps -> TxId -> Int -> Word64 -> IO ()
registerSentTx TxTimestamps{..} id roundNum ts =
    modifyIORef' sentTimes $ M.insert id (roundNum, ts)

splitRound :: [(TxId, (Int, Word64))] -> M.HashMap Int [(TxId, Word64)]
splitRound = foldl' foo M.empty
  where foo m (id, (rnd, ts)) = M.alter (bar id ts) rnd m
        bar id ts mls = Just $ (id, ts) : (concat $ maybeToList mls)

appendVerified :: Word64 -> Int -> [(TxId, Word64)] -> FilePath -> IO ()
appendVerified ts roundNum df logsPrefix = do
    let df' = map (\(id, sts) -> (id, sts, ts)) df
        dfText = mconcat $ map verifyCsvFormat df'
    appendFile (logsPrefix </> verifyCsvFile roundNum) dfText

checkTxsInLastBlock :: forall ssc . SscConstraint ssc
                    => TxTimestamps -> FilePath -> ProductionMode ssc ()
checkTxsInLastBlock TxTimestamps {..} logsPrefix = do
    let lastSafe [] = Nothing
        lastSafe xs = Just $ last xs
    mBlock <- fmap fst . lastSafe <$> loadBlocksFromTipWhile (\_ depth -> depth < k)
    case mBlock of
        Nothing -> pure ()
        Just (Left _) -> pure ()
        Just (Right block) -> do
            st <- liftIO $ readIORef sentTimes
            ls <- liftIO $ readIORef lastSlot
            let curSlot = block^.blockSlot
            when (ls < curSlot) $ do
                let toCheck = M.keys st
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

                let verifiedSentData = map (fromJust . flip M.lookup st) verified
                    verifiedPairedData = zip verified verifiedSentData
                    splitData = splitRound verifiedPairedData

                forM_ (M.toList splitData) $ \(roundNum, df) ->
                    liftIO $ appendVerified (fromIntegral slStart) roundNum df logsPrefix

checkWorker :: forall ssc . SscConstraint ssc
            => TxTimestamps -> FilePath -> ProductionMode ssc ()
checkWorker txts logsPrefix = repeatForever slotDuration onError $
                              checkTxsInLastBlock txts logsPrefix
  where onError e = slotDuration <$
                    logWarning (sformat ("Error occured in checkWorker: " %build) e)
