{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( getHistoryLimited
       , addHistoryTx
       , updateTransaction
       ) where

import           Universum

import qualified Data.DList                 as DL
import qualified Data.Set                   as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logError, logInfo)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Core                   (getTimestamp, timestampToPosix)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, AddrTxFilter (..), CId,
                                             CTx (..), CTxId, CTxMeta (..),
                                             CWAddressMeta (..), Wal, applyAddrTxFilter,
                                             mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo)
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), addOnlyNewTxMeta,
                                             getHistoryCachePart, getPendingTx, getTxMeta,
                                             getWalletPendingTxs, setWalletTxMeta)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getWalletAccountIds, getWalletAddrMetas,
                                             getWalletAddrs, getWalletThTime)


-- | Gather history from all sources: db, mempool txs, pending txs.
-- Note that @minNum@ is just a prompt about minimal number of
-- transactions to return.
getFullWalletHistory
    :: MonadWalletWebMode m
    => CId Wal -> AddrTxFilter -> Int -> m ([CTx], Word)
getFullWalletHistory cWalId filtering minNum = do
    addrs <- mapM decodeCTypeOrFail =<< getWalletAddrs Ever cWalId

    unfilteredLocalHistory <-
        applyAddrTxFilter filtering . DL.toList <$> getLocalHistory addrs

    blockHistory <- getHistoryCachePart filtering minNum cWalId

    let localHistory = filterLocalTh blockHistory unfilteredLocalHistory

    logTxHistory "Block" blockHistory
    logTxHistory "Mempool" localHistory

    fullHistory <- addRecentPtxHistory cWalId $ localHistory <> blockHistory
    cHistory <- forM fullHistory $ addHistoryTx cWalId
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
    -> Int
    -> m ([CTx], Word)
getHistory mCWalId mAccountId mAddrId minNum = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    accAddrs <- map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    cids <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    addrs <- mapM decodeCTypeOrFail cids
    getFullWalletHistory cWalId (AddrTxFilter addrs) minNum
  where
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
    first applySkipLimit <$> getHistory mCWalId mAccId mAddrId (skip + limit)
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
    -> m CTx
addHistoryTx cWalId wtx@THEntry{..} = do
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta <$> case _thTimestamp of
      Nothing -> liftIO $ getPOSIXTime
      Just ts -> return $ fromIntegral (getTimestamp ts) / 1000000
    let cId = encodeCType _thTxId
    addOnlyNewTxMeta cWalId cId meta
    meta' <- fromMaybe meta <$> getTxMeta cWalId cId
    ptxCond <- encodeCType . fmap _ptxCond <$> getPendingTx cWalId _thTxId
    walAddrMetas <- getWalletAddrMetas Ever cWalId
    either (throwM . InternalError) pure $
        mkCTx diff wtx meta' ptxCond walAddrMetas

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
