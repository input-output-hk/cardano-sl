{-# LANGUAGE TypeFamilies #-}

-- | Wallet history

module Pos.Wallet.Web.Methods.History
       ( MonadWalletHistory
       , WalletHistory (..)
       , _WalletHistory
       , WalletHistorySize (..)
       , getHistoryLimited
       , getHistory
       , addHistoryTxMeta
       , constructCTx
       , getCurChainDifficulty
       ) where

import           Universum

import           Control.Lens (makePrisms)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Formatting (sformat, stext, (%))
import           Serokell.Util (listChunkedJson, listJsonIndent)
import           System.Wlog (WithLogger, logDebug)

import           Pos.Client.Txp.History (MonadTxHistory, TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core (Address, ChainDifficulty, HasConfiguration, timestampToPosix)
import           Pos.Core.Txp (TxId)
import           Pos.Util.LogSafe (logInfoSP, secureListF)
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (eitherToThrow)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..), getLocalHistory)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), CTxMeta (..),
                                             ScrollLimit, ScrollOffset, Wal, mkCTx, addressToCId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead)
import           Pos.Wallet.Web.Pending (PendingTx (..), ptxPoolInfo, _PtxApplying)
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (Ever), WalletDB,
                                       WalletSnapshot, addOnlyNewTxMetas, askWalletDB,
                                       getHistoryCache, getPendingTx, getTxMeta,
                                       getWalletPendingTxs, getWalletSnapshot, wamAddress)
import           Pos.Wallet.Web.Util (getAccountAddrsOrThrow, getWalletAccountIds, getWalletAddrs,
                                      getWalletAddrsDetector)
import           Servant.API.ContentTypes (NoContent (..))


type MonadWalletHistory ctx m =
    ( MonadWalletLogicRead ctx m
    , MonadBlockchainInfo m
    , MonadTxHistory m
    )

newtype WalletHistory =
    WalletHistory { unWalletHistory :: Map TxId (CTx, POSIXTime) }

makePrisms ''WalletHistory

newtype WalletHistorySize =
    WalletHistorySize { unWalletHistorySize :: Word }

walletHistorySize :: WalletHistory -> WalletHistorySize
walletHistorySize =
    WalletHistorySize . fromIntegral . Map.size . unWalletHistory

getFullWalletHistory
    :: (MonadWalletHistory ctx m)
    => WalletDB
    -> CId Wal
    -> m (WalletHistory, WalletHistorySize)
getFullWalletHistory db cWalId = do
    logDebug "getFullWalletHistory: start"
    ws <- getWalletSnapshot db

    let addrs = getWalletAddrs ws Ever cWalId

    let blockHistory = getHistoryCache ws cWalId
    unfilteredLocalHistory <- getLocalHistory addrs

    logDebug "getFullWalletHistory: fetched addresses and block/local histories"
    let localHistory = unfilteredLocalHistory `Map.difference` blockHistory

    logTxHistory "Mempool" (toList localHistory)

    fullHistory <- addPtxHistory ws cWalId $ localHistory `Map.union` blockHistory
    let walAddrsDetector = getWalletAddrsDetector ws Ever cWalId
    diff <- getCurChainDifficulty
    logDebug "getFullWalletHistory: fetched full history"

    !cHistory <- WalletHistory <$>
        forM fullHistory (constructCTx ws cWalId walAddrsDetector diff)
    logDebug "getFullWalletHistory: formed cTxs"
    pure (cHistory, walletHistorySize cHistory)

getHistory
    :: MonadWalletHistory ctx m
    => CId Wal
    -> (WalletSnapshot -> [AccountId]) -- ^ Which account IDs to get from the snapshot
    -> Maybe (CId Addr)
    -> m (WalletHistory, WalletHistorySize)
getHistory cWalId getAccIds mAddrId = do
    db <- askWalletDB
    ws <- getWalletSnapshot db

    let allAccIds = getWalletAccountIds ws cWalId
        accIds = getAccIds ws
    -- FIXME: searching when only AddrId is provided is not supported yet.
    accAddrs  <- S.fromList . map (addressToCId . view wamAddress . adiWAddressMeta)
                 <$> concatMapM (getAccountAddrsOrThrow ws Ever) accIds

    let filterFn :: WalletHistory -> Either WalletError WalletHistory
        filterFn cHistory = case mAddrId of
          Nothing
            | S.fromList accIds == S.fromList allAccIds
              -- can avoid doing any expensive filtering in this case
                        -> Right cHistory
            | otherwise -> Right $ filterByAddrs accAddrs cHistory

          Just addr
            | addr `S.member` accAddrs -> Right $ filterByAddrs (S.singleton addr) cHistory
            | otherwise                -> Left errorBadAddress

    (cHistory, cHistorySize) <- getFullWalletHistory db cWalId
    cHistory' <- eitherToThrow $ filterFn cHistory
    logDebug "getHistory: filtered transactions"
    -- TODO: Why do we reuse the old size, pre-filter? Explain.
    return (cHistory', cHistorySize)
  where
    filterByAddrs :: S.Set (CId Addr)
                  -> WalletHistory
                  -> WalletHistory
    filterByAddrs addrs = over _WalletHistory (Map.filter (fits addrs . fst))

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
        (Just cWalId', Nothing) ->
            let accIds' = \ws -> getWalletAccountIds ws cWalId'
             in pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, const [accId])
    (WalletHistory unsortedThs, WalletHistorySize n)
        <- getHistory cWalId accIds mAddrId

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
    :: (MonadIO m, HasConfiguration)
    => WalletDB
    -> CId Wal
    -> TxHistoryEntry
    -> m NoContent
addHistoryTxMeta db cWalId txhe = do
    addHistoryTxsMeta db cWalId . txHistoryListToMap . one $ txhe
    return NoContent

-- This functions is helper to do @addHistoryTx@ for
-- all txs from mempool as one Acidic transaction.
addHistoryTxsMeta
    :: (MonadIO m, HasConfiguration)
    => WalletDB
    -> CId Wal
    -> Map TxId TxHistoryEntry
    -> m ()
addHistoryTxsMeta db cWalId historyEntries = do
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
    -> m (CTx, POSIXTime)
constructCTx ws cWalId addrBelongsToWallet diff wtx@THEntry{..} = do
    let cId = encodeCType _thTxId
    meta <- maybe (CTxMeta <$> liftIO getPOSIXTime) -- It's impossible case but just in case
            pure $ getTxMeta ws cWalId cId
    let ptxCond = encodeCType . fmap _ptxCond $ getPendingTx ws cWalId _thTxId
    either (throwM . InternalError) (pure . (, ctmDate meta)) $
        mkCTx diff wtx meta ptxCond addrBelongsToWallet

getCurChainDifficulty :: MonadBlockchainInfo m => m ChainDifficulty
getCurChainDifficulty = maybe localChainDifficulty pure =<< networkChainDifficulty

addPtxHistory
    :: (MonadIO m, WithLogger m)
    => WalletSnapshot
    -> CId Wal
    -> Map TxId TxHistoryEntry
    -> m (Map TxId TxHistoryEntry)
addPtxHistory ws wid currentHistory = do
    let pendingTxs = fromMaybe [] (getWalletPendingTxs ws wid)
    let conditions = map _ptxCond pendingTxs
    -- show only actually pending transactions in logs
    logTxHistory "Pending" $ mapMaybe (preview _PtxApplying) conditions
    -- but return all transactions which are not yet in blocks
    let candidatesList = txHistoryListToMap (mapMaybe ptxPoolInfo conditions)
    return $ Map.union currentHistory candidatesList

logTxHistory
    :: (WithLogger m, MonadIO m)
    => Text -> [TxHistoryEntry] -> m ()
logTxHistory desc entries = do
    logInfoSP $ \sl ->
        sformat (stext%" transactions history: "%secureListF sl (listChunkedJson 5))
        desc (map _thTxId entries)

logCTxs
    :: (WithLogger m, MonadIO m)
    => Text -> [CTx] -> m ()
logCTxs desc entries =
    logInfoSP $ \sl ->
        sformat (stext%" transactions history: "%secureListF sl (listJsonIndent 4))
        desc (map ctId entries)
