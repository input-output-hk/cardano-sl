{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( getHistoryLimited
       , addHistoryTx
       , constructCTx
       , getCurChainDifficulty
       , updateTransaction
       ) where

import           Universum

import           Control.Exception          (throw)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as S
import           Serokell.Util              (sec)
import           Data.Time.Clock.POSIX      (POSIXTime, getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))
import           Serokell.Util              (listJson, listJsonIndent)
import           System.Wlog                (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core                   (ChainDifficulty, timestampToPosix, Address)
import           Pos.Txp.Core.Types         (TxId, txOutAddress)
import           Pos.Util.LogSafe           (logInfoS)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), CWAddressMeta (..),
                                             ScrollLimit, ScrollOffset, Wal, SinceTime, mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode, convertCIdToAddrs, convertCIdToAddr)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo, _PtxApplying)
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), AddressInfo (..),
                                             addOnlyNewTxMetas, getHistoryCache,
                                             getPendingTx, getTxMeta, getWalletPendingTxs,
                                             setWalletTxMeta)
import           Pos.Wallet.Web.Util        (getAccountAddrsOrThrow, getWalletAccountIds,
                                             getWalletAddrs, getWalletAddrsDetector)

getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m (Map TxId TxHistoryEntry, Word)
getFullWalletHistory cWalId = do
    logDebug "getFullWalletHistory: start"

    cAddrs <- getWalletAddrs Ever cWalId
    addrs <- convertCIdToAddrs cAddrs

    unfilteredLocalHistory <- getLocalHistory addrs

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    logDebug "getFullWalletHistory: fetched addresses and block/local histories"
    let localHistory = unfilteredLocalHistory `Map.difference` blockHistory

    logTxHistory "Mempool" localHistory

    fullHistory <- addRecentPtxHistory cWalId $ localHistory `Map.union` blockHistory
    logDebug "getFullWalletHistory: fetched full history"

    -- TODO when we introduce some mechanism to react on new tx in mempool,
    -- we will set timestamp tx as current time and remove call of @addHistoryTxs@
    -- We call @addHistoryTxs@ only for mempool transactions because for
    -- transactions from block and resubmitting timestamp is already known.
    addHistoryTxs cWalId localHistory
    logDebug "getFullWalletHistory: invoked addHistoryTxs"

    pure (fullHistory, fromIntegral $ Map.size fullHistory)

getFilteredHistory
    :: forall m . MonadWalletWebMode m
    => CId Wal
    -> [AccountId]
    -> Maybe (CId Addr)
    -> m (Map TxId TxHistoryEntry, Word)
getFilteredHistory cWalId accIds mCAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    accCAddrs <- map (cwamId . adiCWAddressMeta) <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    allAccIds <- getWalletAccountIds cWalId

    let !filterFn = case mCAddrId of
          Nothing
            | S.fromList accIds == S.fromList allAccIds
              -- can avoid doing any expensive filtering in this case
                        -> pure
            | otherwise -> \history -> do
                  accAddrs <- S.fromList <$> convertCIdToAddrs accCAddrs
                  pure $ filterByAddrs accAddrs history

          Just caddr
            | caddr `elem` accCAddrs -> \history -> do
                 addr <- convertCIdToAddr caddr
                 pure $ filterByAddrs (S.singleton addr) history
            | otherwise                -> throw errorBadAddress
    res <- (\(x, y) -> (,y) <$> filterFn x) =<< getFullWalletHistory cWalId
    res <$ logDebug "getFilteredHistory: filtered transactions"
  where
    filterByAddrs
        :: S.Set Address
        -> Map TxId TxHistoryEntry
        -> Map TxId TxHistoryEntry
    filterByAddrs addrs = Map.filter (fits addrs)

    fits :: S.Set Address -> TxHistoryEntry -> Bool
    fits addrs THEntry{..} =
        let inpsNOuts = map txOutAddress _thInputs ++ _thOutputAddrs
        in  any (`S.member` addrs) inpsNOuts
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

getHistoryLimited
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe SinceTime
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSince mSkip mLimit = do
    logDebug "getHistoryLimited: started"
    (cWalId, accIds) <- case (mCWalId, mAccId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    (unsortedThs, n) <- getFilteredHistory cWalId accIds mAddrId
    logDebug "getHistoryLimited: invokated getFilteredHistory"

    curTime <- liftIO getPOSIXTime
    let getTxTimestamp entry@THEntry{..} =
            (entry,) . maybe curTime ctmDate <$> getTxMeta cWalId (encodeCType _thTxId)
    txsWithTime <- mapM getTxTimestamp (Map.elems unsortedThs)
    let sinceMicroseconds = sec $ maybe 0 fromIntegral mSince
    let sincePOSIX = timestampToPosix $ fromIntegral sinceMicroseconds
    let sinceTxsWithTime = filter ((> sincePOSIX) . snd) txsWithTime

    let !sortedTxh = forceList $ sortByTime sinceTxsWithTime
    logDebug "getHistoryLimited: sorted transactions"

    let respEntries = applySkipLimit sortedTxh
    walAddrsDetector <- getWalletAddrsDetector Ever cWalId
    diff <- getCurChainDifficulty
    !cHistory <- forM respEntries (constructCTxWithTime cWalId walAddrsDetector diff)
    logDebug "getHistoryLimited: formed cTxs"

    let takeN = min 20 (length cHistory)
    logCTxs (sformat ("Total last "%build) takeN) (take takeN cHistory)
    pure (cHistory, n)
  where
    sortByTime :: [(TxHistoryEntry, POSIXTime)] -> [(TxHistoryEntry, POSIXTime)]
    sortByTime thsWTime =
        -- TODO: if we use a (lazy) heap sort here, we can get the
        -- first n values of the m sorted elements in O(m + n log m)
        sortWith (Down . snd) thsWTime
    forceList l = length l `seq` l
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"

addHistoryTx
    :: MonadWalletWebMode m
    => CId Wal
    -> TxHistoryEntry
    -> m ()
addHistoryTx cWalId = addHistoryTxs cWalId . txHistoryListToMap . one

-- This functions is helper to do @addHistoryTx@ for
-- all txs from mempool as one Acidic transaction.
addHistoryTxs
    :: MonadWalletWebMode m
    => CId Wal
    -> Map TxId TxHistoryEntry
    -> m ()
addHistoryTxs cWalId historyEntries = do
    metas <- mapM toMeta historyEntries
    addOnlyNewTxMetas cWalId metas
  where
    toMeta THEntry {..} = CTxMeta <$> case _thTimestamp of
        Nothing -> liftIO getPOSIXTime
        Just ts -> pure $ timestampToPosix ts

constructCTx
    :: MonadWalletWebMode m
    => CId Wal
    -> (CId Addr -> Bool)
    -> ChainDifficulty
    -> TxHistoryEntry
    -> m CTx
constructCTx cWalId addrBelongsToWallet diff entry@THEntry{..}= do
    let cId = encodeCType _thTxId
    posixTime <- maybe (liftIO getPOSIXTime) (pure . ctmDate) =<< getTxMeta cWalId cId
    constructCTxWithTime cWalId addrBelongsToWallet diff (entry, posixTime)

constructCTxWithTime
    :: MonadWalletWebMode m
    => CId Wal
    -> (CId Addr -> Bool)
    -> ChainDifficulty
    -> (TxHistoryEntry, POSIXTime)
    -> m CTx
constructCTxWithTime cWalId addrBelongsToWallet diff (wtx@THEntry{..}, posixTime) = do
    let meta = CTxMeta posixTime
    ptxCond <- encodeCType . fmap _ptxCond <$> getPendingTx cWalId _thTxId
    either (throwM . InternalError) pure $
        mkCTx diff wtx meta ptxCond addrBelongsToWallet

getCurChainDifficulty :: MonadWalletWebMode m => m ChainDifficulty
getCurChainDifficulty = maybe localChainDifficulty pure =<< networkChainDifficulty

updateTransaction :: MonadWalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta

addRecentPtxHistory
    :: MonadWalletWebMode m
    => CId Wal -> Map TxId TxHistoryEntry -> m (Map TxId TxHistoryEntry)
addRecentPtxHistory wid currentHistory = do
    pendingTxs <- fromMaybe [] <$> getWalletPendingTxs wid
    let conditions = map _ptxCond pendingTxs
    -- show only actually pending transactions in logs
    logTxHistory "Pending" $ mapMaybe (preview _PtxApplying) conditions
    -- but return all transactions which are not yet in blocks
    let candidatesList = txHistoryListToMap (mapMaybe ptxPoolInfo conditions)
    return $ Map.union currentHistory candidatesList

-- FIXME: use @listChunkedJson k@ with appropriate @k@s, once available,
-- in these 2 functions
logTxHistory
    :: (Container t, Element t ~ TxHistoryEntry, WithLogger m, MonadIO m)
    => Text -> t -> m ()
logTxHistory desc =
    logInfoS .
    sformat (stext%" transactions history: "%listJson) desc .
    map _thTxId . toList

logCTxs
    :: (Container t, Element t ~ CTx, WithLogger m)
    => Text -> t -> m ()
logCTxs desc =
    logInfo .
    sformat (stext%" transactions history: "%listJsonIndent 4) desc .
    map ctId . toList
