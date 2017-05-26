{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TxAnalysis
       ( createTxTimestamps
       , registerSentTx
       , checkWorker
       ) where

import           Universum             hiding (catchAll)

import           Control.Lens          (_Wrapped, _last)
import qualified Data.HashMap.Strict   as M
import           Data.List             (intersect)
import           Data.Maybe            (fromJust)
import           Formatting            (build, sformat, (%))
import           Mockable              (catchAll, delay)
import           System.FilePath.Posix ((</>))
import           System.Wlog           (logWarning)

import           Pos.Block.Core        (mainBlockSlot, mainBlockTxPayload)
import           Pos.Constants         (blkSecurityParam, genesisSlotDuration)
import           Pos.Core              (SlotId (..))
import           Pos.Crypto            (hash)
import           Pos.DB.DB             (loadBlundsFromTipByDepth)
import           Pos.Slotting          (getCurrentSlotBlocking, getSlotStartEmpatically)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Txp               (TxId, txpTxs)
import           Pos.WorkMode          (StaticMode)

import           Util                  (verifyCsvFile, verifyCsvFormat)

type TxTimeMap = M.HashMap TxId (Int, Word64)

data TxTimestamps = TxTimestamps
    { sentTimes :: IORef TxTimeMap
    , lastSlot  :: IORef SlotId
    }

createTxTimestamps :: IO TxTimestamps
createTxTimestamps = TxTimestamps
                     <$> newIORef M.empty
                     <*> newIORef (SlotId 0 minBound)

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

checkTxsInLastBlock
    :: forall ssc.
       SscConstraint ssc
    => TxTimestamps -> FilePath -> StaticMode ssc ()
checkTxsInLastBlock TxTimestamps {..} logsPrefix = do
    mBlock <- preview (_Wrapped . _last . _1) <$>
              loadBlundsFromTipByDepth @ssc blkSecurityParam
    case mBlock of
        Nothing -> pure ()
        Just (Left _) -> pure ()
        Just (Right block) -> do
            st <- readIORef sentTimes
            ls <- readIORef lastSlot
            let curSlot = block ^. mainBlockSlot
            when (ls < curSlot) $ do
                let toCheck = M.keys st
                    txsMerkle = block ^. mainBlockTxPayload . txpTxs
                    txIds = map hash $ toList txsMerkle
                    verified = toCheck `intersect` txIds

                -- Delete verified txs from hashmap
                let newSt = foldr M.delete st verified
                writeIORef sentTimes newSt

                -- We don't know exact time when checked block has been
                -- created/adopted, but we do know that it was not at
                -- `blkSecurityParam` depth a slot ago, so we just take a
                -- beginning of current slot
                slStart <- getSlotStartEmpatically =<< getCurrentSlotBlocking
                writeIORef lastSlot curSlot

                let verifiedSentData = map (fromJust . flip M.lookup st) verified
                    verifiedPairedData = zip verified verifiedSentData
                    splitData = splitRound verifiedPairedData

                for_ (M.toList splitData) $ \(roundNum, df) ->
                    liftIO $ appendVerified (fromIntegral slStart) roundNum df logsPrefix

checkWorker :: forall ssc . SscConstraint ssc
            => TxTimestamps -> FilePath -> StaticMode ssc ()
checkWorker txts logsPrefix = loop `catchAll` onError
  where
    loop = do
        checkTxsInLastBlock txts logsPrefix
        delay genesisSlotDuration
        loop
    onError e = do
        logWarning (sformat ("Error occured in checkWorker: " %build) e)
        delay genesisSlotDuration
        loop
