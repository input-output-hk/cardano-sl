{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( getHistoryLimited
       , addHistoryTx
       , updateTransaction
       ) where

import           Universum

import qualified Data.DList                 as DL
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logError, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Core                   (getTimestamp, timestampToPosix)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.WalletMode      (getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxId,
                                             CTxMeta (..), CWAddressMeta (..), Wal, mkCTx)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending     (PendingTx (..), ptxPoolInfo)
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), addOnlyNewTxMeta,
                                             getHistoryCache, getPendingTx, getTxMeta,
                                             getWalletPendingTxs, setWalletTxMeta)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getWalletAccountIds, getWalletAddrMetas,
                                             getWalletAddrs, getWalletThTime)


getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM decodeCTypeOrFail =<< getWalletAddrs Ever cWalId

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure $ DL.fromList hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    localHistory <- getLocalHistory addrs

    fullHistory <- addRecentPtxHistory cWalId $ DL.toList $ localHistory <> blockHistory
    cHistory <- forM fullHistory $ addHistoryTx cWalId
    pure (cHistory, fromIntegral $ length cHistory)

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
    accAddrs <- map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    addrs <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    first (filter (fits addrs)) <$> getFullWalletHistory cWalId
  where
    fits :: [CId Addr] -> CTx -> Bool
    fits addrs ctx = any (relatesToAddr ctx) addrs
    relatesToAddr CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))
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

