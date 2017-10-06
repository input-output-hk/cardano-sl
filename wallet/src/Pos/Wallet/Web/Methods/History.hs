{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( getHistoryLimited
       , addHistoryTx
       , constructCTx
       , updateTransaction
       ) where

import           Universum

import qualified Data.DList                 as DL
import qualified Data.HashSet               as HS
import qualified Data.Set                   as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logError, logInfo, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Core                   (ChainDifficulty, timestampToPosix)
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
                                             getWalletAddrs, getWalletThTime)


getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM decodeCTypeOrFail =<< getWalletAddrs Ever cWalId

    unfilteredLocalHistory <- getLocalHistory addrs

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure $ DL.fromList hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    let localHistory =
            DL.fromList $ filterLocalTh
                (DL.toList blockHistory)
                (DL.toList unfilteredLocalHistory)

    logTxHistory "Block" blockHistory
    logTxHistory "Mempool" localHistory

    fullHistory <- addRecentPtxHistory cWalId $ DL.toList $ localHistory <> blockHistory
    txCData <- getCurTxConstructionData cWalId
    -- TODO when we introduce some mechanism to react on new tx in mempool,
    -- we will set timestamp tx as current time and remove call of @addHistoryTxs@
    -- We call @addHistoryTxs@ only for mempool transactions because for
    -- transactions from block and resubmitting timestamp is already known.
    addHistoryTxs cWalId (DL.toList localHistory)
    cHistory <- forM fullHistory (constructCTx (cWalId, Just txCData))
    pure (cHistory, fromIntegral $ length cHistory)
  where
    filterLocalTh :: [TxHistoryEntry] -> [TxHistoryEntry] -> [TxHistoryEntry]
    filterLocalTh blockH localH =
        let blockTxIdsSet = S.fromList $ map _thTxId blockH
        in  filter ((`S.notMember` blockTxIdsSet) . _thTxId) localH

getHistory
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> m ([CTx], Word)
getHistory mCWalId mAccountId mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    allAccIds <- getWalletAccountIds cWalId

    let getAccAddrs = map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
        noAccFiltering = HS.fromList accIds == HS.fromList allAccIds

    filterFunction <- case mAddrId of
        Nothing -> if noAccFiltering
            then pure identity
            else filterByAddrs <$> getAccAddrs
        Just addr -> do
            accAddrs <- getAccAddrs
            if addr `elem` accAddrs
                then pure $ filterByAddrs [addr]
                else throwM errorBadAddress

    first filterFunction <$> getFullWalletHistory cWalId
  where
    fits :: S.Set (CId Addr) -> CTx -> Bool
    fits addrs CTx{..} =
        let inpsNOuts = map fst (ctInputs ++ ctOutputs)
        in  any (`S.member` addrs) inpsNOuts
    filterByAddrs = filter . fits . S.fromList
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"
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
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit =
    first applySkipLimit <$> getHistory mCWalId mAccId mAddrId
  where
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0

addHistoryTx
    :: MonadWalletWebMode m
    => CId Wal
    -> TxHistoryEntry
    -> m ()
addHistoryTx cWalId = addHistoryTxs cWalId . one

-- This functions is helper to do @addHistoryTx@ for
-- all txs from mempool as one Acidic transaction.
addHistoryTxs
    :: MonadWalletWebMode m
    => CId Wal
    -> [TxHistoryEntry]
    -> m ()
addHistoryTxs cWalId historyEntries = do
    metas <- mapM toMeta historyEntries
    let cIds = map (encodeCType . _thTxId) historyEntries
    addOnlyNewTxMetas cWalId (zip cIds metas)
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
    -> m CTx
constructCTx (cWalId, mTxData) wtx@THEntry{..} = do
    TxConstructionData {..} <- maybe (getCurTxConstructionData cWalId) pure mTxData
    let cId = encodeCType _thTxId
    meta <- maybe (CTxMeta <$> liftIO getPOSIXTime) -- It's impossible case but just in case
            pure =<< getTxMeta cWalId cId
    ptxCond <- encodeCType . fmap _ptxCond <$> getPendingTx cWalId _thTxId
    either (throwM . InternalError) pure $
        mkCTx tcdChainDiff wtx meta ptxCond tcdWAddrsSet

updateTransaction :: MonadWalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta

addRecentPtxHistory
    :: MonadWalletWebMode m
    => CId Wal -> [TxHistoryEntry] -> m [TxHistoryEntry]
addRecentPtxHistory wid currentHistory = do
    candidates <- sortWith (Down . _thTimestamp) <$> getCandidates
    logTxHistory "Pending" candidates
    merge currentHistory candidates
  where
    getCandidates =
        mapMaybe (ptxPoolInfo . _ptxCond) . fromMaybe [] <$>
        getWalletPendingTxs wid

    merge [] recent     = return recent
    merge current []    = return current
    merge (c:cs) (r:rs) = do
        if _thTxId c == _thTxId r
            then (c:) <$> merge cs rs
            else do
                mctime <- getWalletThTime wid c
                let mrtime = timestampToPosix <$> _thTimestamp r
                when (isNothing mrtime) $ reportNoTimestamp r
                if mctime <= mrtime
                    then (r:) <$> merge (c:cs) rs
                    else (c:) <$> merge cs (r:rs)

    -- pending transactions are made by us, always have timestamp set
    reportNoTimestamp th =
        logError $
        sformat ("Pending transaction "%build%" has no timestamp set")
                (_thTxId th)


logTxHistory
    :: (Container t, Element t ~ TxHistoryEntry, WithLogger m)
    => Text -> t -> m ()
logTxHistory desc =
    logInfo .
    sformat (stext%" transactions history: "%listJson) desc .
    map _thTxId . toList
