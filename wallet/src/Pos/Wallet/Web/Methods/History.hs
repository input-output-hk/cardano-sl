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
import           Pos.Core                   (ChainDifficulty, timestampToPosix)
import           Pos.Txp.Core.Types         (TxId)
import           Pos.Util.LogSafe           (logInfoS)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), CWAddressMeta (..),
                                             ScrollLimit, ScrollOffset, Wal, mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode, convertCIdTOAddrs)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo, _PtxApplying)
import           Pos.Wallet.Web.State       (AddressInfo (..), AddressLookupMode (Ever),
                                             WalletDB, WalletSnapshot, addOnlyNewTxMetas,
                                             askWalletDB, getHistoryCache, getPendingTx,
                                             getTxMeta, getWalletPendingTxs,
                                             getWalletSnapshot, setWalletTxMeta)
import           Pos.Wallet.Web.Util        (getAccountAddrsOrThrow, getWalletAccountIds,
                                             getWalletAddrs, getWalletAddrsDetector)

getFullWalletHistory :: (MonadIO m, MonadThrow m, WithLogger m)
                     => WalletDB
                     -> CId Wal
                     -> (Map TxId TxHistoryEntry)
                     -> ChainDifficulty
                     -> m (Map TxId (CTx, POSIXTime), Word)
getFullWalletHistory db cWalId unfilteredLocalHistory diff = do
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
    let walAddrsDetector = getWalletAddrsDetector ws Ever cWalId
    logDebug "getFullWalletHistory: fetched full history"

    -- TODO when we introduce some mechanism to react on new tx in mempool,
    -- we will set timestamp tx as current time and remove call of @addHistoryTxs@
    -- We call @addHistoryTxs@ only for mempool transactions because for
    -- transactions from block and resubmitting timestamp is already known.
    -- XXX rewrite 'getHistory'
    addHistoryTxs db cWalId localHistory
    logDebug "getFullWalletHistory: invoked addHistoryTxs"

    ws' <- getWalletSnapshot db
    !cHistory <- forM fullHistory (constructCTx ws' cWalId walAddrsDetector diff)
    logDebug "getFullWalletHistory: formed cTxs"
    pure (cHistory, fromIntegral $ Map.size cHistory)

getHistory
    :: MonadWalletWebMode m
    => CId Wal
    -> (WalletSnapshot -> [AccountId]) -- ^ Which account IDs to get from the snapshot
    -> Maybe (CId Addr)
    -> m (Map TxId (CTx, POSIXTime), Word)
getHistory cWalId getAccIds mAddrId = do
    db <- askWalletDB
    ws <- getWalletSnapshot db

    let allAccIds = getWalletAccountIds ws cWalId
        accIds = getAccIds ws

    -- FIXME: searching when only AddrId is provided is not supported yet.
    accAddrs  <- S.fromList . map (cwamId . adiCWAddressMeta) <$> concatMapM (getAccountAddrsOrThrow ws Ever) accIds

    let filterFn :: Map TxId (CTx, POSIXTime) -> Map TxId (CTx, POSIXTime)
        !filterFn = case mAddrId of
          Nothing
            | S.fromList accIds == S.fromList allAccIds
              -- can avoid doing any expensive filtering in this case
                        -> identity
            | otherwise -> filterByAddrs accAddrs

          Just addr
            | addr `S.member` accAddrs -> filterByAddrs (S.singleton addr)
            | otherwise                -> throw errorBadAddress

    let cAddrs = getWalletAddrs ws Ever cWalId

    unfilteredLocalHistory <- getLocalHistory =<< convertCIdTOAddrs cAddrs
    diff        <- getCurChainDifficulty

    res <- first filterFn <$> getFullWalletHistory db cWalId unfilteredLocalHistory diff
    logDebug "getHistory: filtered transactions"
    return res
  where
    filterByAddrs :: S.Set (CId Addr)
                  -> Map TxId (CTx, POSIXTime)
                  -> Map TxId (CTx, POSIXTime)
    filterByAddrs addrs = Map.filter (fits addrs . fst)

    fits :: S.Set (CId Addr) -> CTx -> Bool
    fits addrs CTx{..} =
        let inpsNOuts = map fst (ctInputs ++ ctOutputs)
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
    (cWalId, accIds) <- case (mCWalId, mAccId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) ->
            let accIds' = \ws -> getWalletAccountIds ws cWalId'
             in pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, const [accId])
    (unsortedThs, n) <- getHistory cWalId accIds mAddrId

    let !sortedTxh = forceList $ sortByTime (Map.elems unsortedThs)
    logDebug "getHistoryLimited: sorted transactions"

    logCTxs "Total last 20" $ take 20 sortedTxh
    pure (applySkipLimit sortedTxh, n)
  where
    sortByTime :: [(CTx, POSIXTime)] -> [CTx]
    sortByTime thsWTime =
        -- TODO: if we use a (lazy) heap sort here, we can get the
        -- first n values of the m sorted elements in O(m + n log m)
        map fst $ sortWith (Down . snd) thsWTime
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
    -> (CId Addr -> Bool)
    -> ChainDifficulty
    -> TxHistoryEntry
    -> m (CTx, POSIXTime)
constructCTx ws cWalId addrBelongsToWallet diff wtx@THEntry{..} = do
    let cId = encodeCType _thTxId
    meta <- maybe (CTxMeta <$> liftIO getPOSIXTime) -- It's impossible case but just in case
            pure $ getTxMeta ws cWalId cId
    let ptxCond = encodeCType . fmap _ptxCond $ getPendingTx ws cWalId _thTxId
    either (throwM . InternalError) (pure . (, ctmDate meta)) $
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
