{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( getHistoryLimited
       , addHistoryTx
       , constructCTx
       , updateTransaction
       ) where

import           Universum

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as S
import           Data.Time.Clock.POSIX      (POSIXTime, getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logInfo, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core                   (ChainDifficulty, timestampToPosix)
import           Pos.Txp.Core.Types         (TxId)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), CWAddressMeta (..), Wal, mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo)
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), addOnlyNewTxMetas,
                                             getHistoryCache, getPendingTx, getTxMeta,
                                             getWalletPendingTxs, setWalletTxMeta)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getWalletAccountIds, getWalletAddrMetas,
                                             getWalletAddrs)


getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m (Map TxId (CTx, POSIXTime), Word)
getFullWalletHistory cWalId = do
    addrs <- mapM decodeCTypeOrFail =<< getWalletAddrs Ever cWalId

    unfilteredLocalHistory <- getLocalHistory addrs

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    let localHistory = unfilteredLocalHistory `Map.difference` blockHistory

    logTxHistory "Block" blockHistory
    logTxHistory "Mempool" localHistory

    fullHistory <- addRecentPtxHistory cWalId $ localHistory `Map.union` blockHistory
    txCData <- getCurTxConstructionData cWalId
    -- TODO when we introduce some mechanism to react on new tx in mempool,
    -- we will set timestamp tx as current time and remove call of @addHistoryTxs@
    -- We call @addHistoryTxs@ only for mempool transactions because for
    -- transactions from block and resubmitting timestamp is already known.
    addHistoryTxs cWalId localHistory
    cHistory <- forM fullHistory (constructCTx (cWalId, Just txCData))
    pure (cHistory, fromIntegral $ Map.size cHistory)

getHistory
    :: MonadWalletWebMode m
    => CId Wal
    -> [AccountId]
    -> Maybe (CId Addr)
    -> m (Map TxId (CTx, POSIXTime), Word)
getHistory cWalId accIds mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    allAccIds <- getWalletAccountIds cWalId

    let getAccAddrs = map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
        noAccFiltering = S.fromList accIds == S.fromList allAccIds

    filterFunction <- case mAddrId of
        Nothing -> if noAccFiltering
            then pure identity
            else filterByAddrs <$> getAccAddrs
        Just addr -> do
            accAddrs <- getAccAddrs
            if addr `elem` accAddrs
                then pure $ filterByAddrs [addr]
                else throwM errorBadAddress

    first filterFunction <$> (getFullWalletHistory cWalId)
  where
    fits :: S.Set (CId Addr) -> CTx -> Bool
    fits addrs CTx{..} =
        let inpsNOuts = map fst (ctInputs ++ ctOutputs)
        in  any (`S.member` addrs) inpsNOuts
    filterByAddrs addrs = Map.filter (fits (S.fromList addrs) . fst)
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

getHistoryLimited
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit = do
    (cWalId, accIds) <- case (mCWalId, mAccId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    (unsortedThs, n) <- getHistory cWalId accIds mAddrId
    let sortedTxh = sortByTime (Map.elems unsortedThs)
    pure (applySkipLimit sortedTxh, n)
  where
    sortByTime :: [(CTx, POSIXTime)] -> [CTx]
    sortByTime thsWTime =
        -- TODO: if we use a (lazy) heap sort here, we can get the
        -- first n values of the m sorted elements in O(m + n log m)
        map fst $ sortWith (Down . snd) thsWTime
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

-- | A datatype containing data which is needed
-- to create a tx history entry but can be computed
-- only once for all entries
data TxConstructionData = TxConstructionData
    { tcdWAddrsSet :: !(Set (CId Addr))
    , tcdChainDiff :: !ChainDifficulty
    }

getCurTxConstructionData
    :: MonadWalletWebMode m
    => CId Wal
    -> m TxConstructionData
getCurTxConstructionData cWalId = do
    tcdWAddrsSet <- S.fromList . map cwamId <$> getWalletAddrMetas Ever cWalId
    tcdChainDiff <- maybe localChainDifficulty pure =<< networkChainDifficulty
    pure TxConstructionData {..}

constructCTx
    :: MonadWalletWebMode m
    => (CId Wal, Maybe TxConstructionData)
    -> TxHistoryEntry
    -> m (CTx, POSIXTime)
constructCTx (cWalId, mTxData) wtx@THEntry{..} = do
    TxConstructionData {..} <- maybe (getCurTxConstructionData cWalId) pure mTxData
    let cId = encodeCType _thTxId
    meta <- maybe (CTxMeta <$> liftIO getPOSIXTime) -- It's impossible case but just in case
            pure =<< getTxMeta cWalId cId
    ptxCond <- encodeCType . fmap _ptxCond <$> getPendingTx cWalId _thTxId
    either (throwM . InternalError) (pure . (, ctmDate meta)) $
        mkCTx tcdChainDiff wtx meta ptxCond tcdWAddrsSet

updateTransaction :: MonadWalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta

addRecentPtxHistory
    :: MonadWalletWebMode m
    => CId Wal -> Map TxId TxHistoryEntry -> m (Map TxId TxHistoryEntry)
addRecentPtxHistory wid currentHistory = do
    pendingTxs <- getWalletPendingTxs wid
    let candidates = toCandidates pendingTxs
    logTxHistory "Pending" candidates
    return $ Map.union currentHistory candidates
  where
    toCandidates =
            txHistoryListToMap
        .   mapMaybe (ptxPoolInfo . _ptxCond)
        .   fromMaybe []

logTxHistory
    :: (Container t, Element t ~ TxHistoryEntry, WithLogger m)
    => Text -> t -> m ()
logTxHistory desc =
    logInfo .
    sformat (stext%" transactions history: "%listJson) desc .
    map _thTxId . toList
