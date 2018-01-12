{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Data.Time.Clock.POSIX      (POSIXTime, getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))
import           Serokell.Util              (listJson, listJsonIndent)
import           System.Wlog                (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core                   (Address, ChainDifficulty, timestampToPosix)
import           Pos.Txp.Core.Types         (TxId, txOutAddress)
import           Pos.Util.LogSafe           (logInfoS)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), ScrollLimit, ScrollOffset, Wal,
                                             mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo, _PtxApplying)
import           Pos.Wallet.Web.State       (AddressInfo (..), AddressLookupMode (Ever),
                                             WalletDB, WalletSnapshot, addOnlyNewTxMetas,
                                             askWalletDB, getHistoryCache, getPendingTx,
                                             getTxMeta, getWalletPendingTxs,
                                             getWalletSnapshot, setWalletTxMeta,
                                             wamAddress)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getWalletAccountIds, getWalletAddrs,
                                             getWalletAddrsDetector)

getFullWalletHistory :: (MonadIO m, MonadThrow m, WithLogger m)
                     => WalletDB
                     -> CId Wal
                     -> (Map TxId TxHistoryEntry)
                     -> m (Map TxId TxHistoryEntry, Word)
getFullWalletHistory db cWalId unfilteredLocalHistory = do
    logDebug "getFullWalletHistory: start"
    ws <- getWalletSnapshot db

    blockHistory <- case getHistoryCache ws cWalId of
        Just hist -> pure hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    logDebug "getFullWalletHistory: fetched addresses and block/local histories"
    let localHistory = unfilteredLocalHistory `Map.difference` blockHistory

    logTxHistory "Mempool" localHistory

    fullHistory <- addRecentPtxHistory ws cWalId $ localHistory `Map.union` blockHistory
    logDebug "getFullWalletHistory: fetched full history"

    -- TODO when we introduce some mechanism to react on new tx in mempool,
    -- we will set timestamp tx as current time and remove call of @addHistoryTxs@
    -- We call @addHistoryTxs@ only for mempool transactions because for
    -- transactions from block and resubmitting timestamp is already known.
    addHistoryTxs db cWalId localHistory
    logDebug "getFullWalletHistory: invoked addHistoryTxs"

    pure (fullHistory, fromIntegral $ Map.size fullHistory)

getFilteredHistory
    :: MonadWalletWebMode m
    => WalletDB
    -> WalletSnapshot
    -> CId Wal
    -> (WalletSnapshot -> [AccountId]) -- ^ Which account IDs to get from the snapshot
    -> Maybe (CId Addr)
    -> m (Map TxId TxHistoryEntry, Word)
getFilteredHistory db ws cWalId getAccIds mCAddrId = do

    let allAccIds = getWalletAccountIds ws cWalId
        accIds = getAccIds ws

    -- FIXME: searching when only AddrId is provided is not supported yet.
    accCAddrs <- map (view wamAddress . adiWAddressMeta) <$> concatMapM (getAccountAddrsOrThrow ws Ever) accIds

    !filterFn <- case mCAddrId of
          Nothing
            | S.fromList accIds == S.fromList allAccIds
              -- can avoid doing any expensive filtering in this case
                        -> return pure
            | otherwise -> return $ \history -> do
                  let accAddrs = S.fromList accCAddrs
                  pure $ filterByAddrs accAddrs history

          Just caddr -> do
            addr <- decodeCTypeOrFail caddr
            unless (addr `elem` accCAddrs) $
                throw errorBadAddress
            return $ \history -> do
                 pure $ filterByAddrs (S.singleton addr) history

    let addrs = getWalletAddrs ws Ever cWalId
    unfilteredLocalHistory <- getLocalHistory addrs

    res <- (\(x, y) -> (,y) <$> filterFn x) =<< getFullWalletHistory db cWalId unfilteredLocalHistory
    logDebug "getFilteredHistory: filtered transactions"
    return res
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
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit = do
    logDebug "getHistoryLimited: started"
    db <- askWalletDB
    ws <- getWalletSnapshot db

    (cWalId, accIds) <- case (mCWalId, mAccId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) ->
            let accIds' = \ws' -> getWalletAccountIds ws' cWalId'
             in pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, const [accId])
    (unsortedThs, n) <- getFilteredHistory db ws cWalId accIds mAddrId
    logDebug "getHistoryLimited: invokated getFilteredHistory"

    curTime <- liftIO getPOSIXTime
    let getTxTimestamp entry@THEntry{..} =
            (entry,) . maybe curTime ctmDate $ getTxMeta ws cWalId (encodeCType _thTxId)
        txsWithTime = map getTxTimestamp (Map.elems unsortedThs)
        !sortedTxh = forceList $ sortByTime txsWithTime
    logDebug "getHistoryLimited: sorted transactions"

    let respEntries = applySkipLimit sortedTxh
        walAddrsDetector = getWalletAddrsDetector ws Ever cWalId
    diff <- getCurChainDifficulty
    !cHistory <- forM respEntries (constructCTxWithTime ws cWalId walAddrsDetector diff)
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
    :: MonadIO m
    => WalletDB
    -> CId Wal
    -> TxHistoryEntry
    -> m ()
addHistoryTx db cWalId = addHistoryTxs db cWalId . txHistoryListToMap . one

-- This functions is helper to do @addHistoryTx@ for
-- all txs from mempool as one Acidic transaction.
addHistoryTxs
    :: MonadIO m
    => WalletDB
    -> CId Wal
    -> Map TxId TxHistoryEntry
    -> m ()
addHistoryTxs db cWalId historyEntries = do
    metas <- mapM toMeta historyEntries
    addOnlyNewTxMetas db cWalId metas
  where
    toMeta THEntry {..} = CTxMeta <$> case _thTimestamp of
        Nothing -> liftIO getPOSIXTime
        Just ts -> pure $ timestampToPosix ts

constructCTx
    :: (MonadIO m, MonadThrow m)
    => WalletSnapshot
    -> CId Wal
    -> (Address -> Bool)
    -> ChainDifficulty
    -> TxHistoryEntry
    -> m CTx
constructCTx ws cWalId addrBelongsToWallet diff entry@THEntry{..}= do
    let cId = encodeCType _thTxId
    posixTime <- maybe (liftIO getPOSIXTime) (pure . ctmDate) $ getTxMeta ws cWalId cId
    constructCTxWithTime ws cWalId addrBelongsToWallet diff (entry, posixTime)

constructCTxWithTime
    :: MonadThrow m
    => WalletSnapshot
    -> CId Wal
    -> (Address -> Bool)
    -> ChainDifficulty
    -> (TxHistoryEntry, POSIXTime)
    -> m CTx
constructCTxWithTime ws cWalId addrBelongsToWallet diff (wtx@THEntry{..}, posixTime) = do
    let meta = CTxMeta posixTime
        ptxCond = encodeCType . fmap _ptxCond $ getPendingTx ws cWalId _thTxId
    either (throwM . InternalError) pure $
        mkCTx diff wtx meta ptxCond addrBelongsToWallet

getCurChainDifficulty :: MonadWalletWebMode m => m ChainDifficulty
getCurChainDifficulty = maybe localChainDifficulty pure =<< networkChainDifficulty

updateTransaction :: MonadWalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    db <- askWalletDB
    setWalletTxMeta db (aiWId accId) txId txMeta

addRecentPtxHistory
    :: (WithLogger m, MonadIO m)
    => WalletSnapshot
    -> CId Wal
    -> Map TxId TxHistoryEntry
    -> m (Map TxId TxHistoryEntry)
addRecentPtxHistory ws wid currentHistory = do
    let pendingTxs = fromMaybe [] (getWalletPendingTxs ws wid)
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
