{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( MonadWalletHistory
       , getHistoryLimited
       , addHistoryTxMeta
       , constructCTx
       , getCurChainDifficulty
       , updateTransaction
       ) where

import           Universum

import           Control.Exception.Safe (impureThrow)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Formatting (build, sformat, stext, (%))
import           Serokell.Util (listJson, listJsonIndent)
import           System.Wlog (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Client.Txp.History (MonadTxHistory, TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core (ChainDifficulty, timestampToPosix)
import           Pos.Core.Txp (TxId)
import           Pos.Util.LogSafe (logInfoS)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..), getLocalHistory)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), CWAddressMeta (..), ScrollLimit,
                                             ScrollOffset, Wal, mkCTx)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead)
import           Pos.Wallet.Web.Methods.Misc (convertCIdTOAddrs)
import           Pos.Wallet.Web.Pending (PendingTx (..), ptxPoolInfo, _PtxApplying)
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (Ever), MonadWalletDB,
                                       MonadWalletDBRead, addOnlyNewTxMetas, getHistoryCache,
                                       getPendingTx, getTxMeta, getWalletPendingTxs,
                                       setWalletTxMeta)
import           Pos.Wallet.Web.Util (getAccountAddrsOrThrow, getWalletAccountIds, getWalletAddrs,
                                      getWalletAddrsDetector)
import           Servant.API.ContentTypes (NoContent (..))


type MonadWalletHistory ctx m =
    ( MonadWalletLogicRead ctx m
    , MonadBlockchainInfo m
    , MonadTxHistory m
    )

getFullWalletHistory
    :: MonadWalletHistory ctx m
    => CId Wal -> m (Map TxId (CTx, POSIXTime), Word)
getFullWalletHistory cWalId = do
    logDebug "getFullWalletHistory: start"

    cAddrs <- getWalletAddrs Ever cWalId
    addrs <- convertCIdTOAddrs cAddrs

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

    fullHistory <- addPtxHistory cWalId $ localHistory `Map.union` blockHistory
    walAddrsDetector <- getWalletAddrsDetector Ever cWalId
    diff <- getCurChainDifficulty
    logDebug "getFullWalletHistory: fetched full history"

    !cHistory <- forM fullHistory (constructCTx cWalId walAddrsDetector diff)
    logDebug "getFullWalletHistory: formed cTxs"
    pure (cHistory, fromIntegral $ Map.size cHistory)

getHistory
    :: MonadWalletHistory ctx m
    => CId Wal
    -> [AccountId]
    -> Maybe (CId Addr)
    -> m (Map TxId (CTx, POSIXTime), Word)
getHistory cWalId accIds mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    accAddrs  <- S.fromList . map (cwamId . adiCWAddressMeta) <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    allAccIds <- getWalletAccountIds cWalId

    let filterFn :: Map TxId (CTx, POSIXTime) -> Map TxId (CTx, POSIXTime)
        !filterFn = case mAddrId of
          Nothing
            | S.fromList accIds == S.fromList allAccIds
              -- can avoid doing any expensive filtering in this case
                        -> identity
            | otherwise -> filterByAddrs accAddrs

          Just addr
            | addr `S.member` accAddrs -> filterByAddrs (S.singleton addr)
            | otherwise                -> impureThrow errorBadAddress

    res <- first filterFn <$> getFullWalletHistory cWalId
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
    :: MonadWalletHistory ctx m
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
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
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

addHistoryTxMeta
    :: MonadWalletDB ctx m
    => CId Wal
    -> TxHistoryEntry
    -> m NoContent
addHistoryTxMeta cWalId txhe = do
    _ <- addHistoryTxsMeta cWalId . txHistoryListToMap . one $ txhe
    return NoContent

-- This functions is helper to do @addHistoryTx@ for
-- all txs from mempool as one Acidic transaction.
addHistoryTxsMeta
    :: MonadWalletDB ctx m
    => CId Wal
    -> Map TxId TxHistoryEntry
    -> m NoContent
addHistoryTxsMeta cWalId historyEntries = do
    metas <- mapM toMeta historyEntries
    addOnlyNewTxMetas cWalId metas
    return NoContent
  where
    toMeta THEntry {..} = CTxMeta <$> case _thTimestamp of
        Nothing -> liftIO getPOSIXTime
        Just ts -> pure $ timestampToPosix ts

constructCTx
    :: (MonadThrow m, MonadWalletDBRead ctx m)
    => CId Wal
    -> (CId Addr -> Bool)
    -> ChainDifficulty
    -> TxHistoryEntry
    -> m (CTx, POSIXTime)
constructCTx cWalId addrBelongsToWallet diff wtx@THEntry{..} = do
    let cId = encodeCType _thTxId
    meta <- maybe (CTxMeta <$> liftIO getPOSIXTime) -- It's impossible case but just in case
            pure =<< getTxMeta cWalId cId
    ptxCond <- encodeCType . fmap _ptxCond <$> getPendingTx cWalId _thTxId
    either (throwM . InternalError) (pure . (, ctmDate meta)) $
        mkCTx diff wtx meta ptxCond addrBelongsToWallet

getCurChainDifficulty :: MonadBlockchainInfo m => m ChainDifficulty
getCurChainDifficulty = maybe localChainDifficulty pure =<< networkChainDifficulty

updateTransaction :: MonadWalletDB ctx m => AccountId -> CTxId -> CTxMeta -> m NoContent
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta
    return NoContent

addPtxHistory
    :: (WithLogger m, MonadWalletDBRead ctx m)
    => CId Wal -> Map TxId TxHistoryEntry -> m (Map TxId TxHistoryEntry)
addPtxHistory wid currentHistory = do
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
logTxHistory desc = do
    logInfoS
        . sformat (stext%" transactions history: "%listJson) desc
        . map _thTxId
        . toList

logCTxs
    :: (Container t, Element t ~ CTx, WithLogger m)
    => Text -> t -> m ()
logCTxs desc =
    logInfo .
    sformat (stext%" transactions history: "%listJsonIndent 4) desc .
    map ctId . toList
