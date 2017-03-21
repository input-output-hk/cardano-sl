{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- API server logic

module Pos.Explorer.Web.Server
       ( explorerServeImpl
       , explorerApp
       , explorerHandlers
       ) where

import           Control.Lens                   (at)
import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Either     (EitherT (..))
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Either.Combinators        (swapEither)
import qualified Data.HashMap.Strict            as HM
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)
import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), hash, withHash)
import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.GState                  as GS
import qualified Pos.DB.GState.Balances         as GS (getFtsStake)
import qualified Pos.DB.GState.Explorer         as GS (getAddrHistory,
                                                       getTxExtra)
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), TxAux, TxId,
                                                 TxOutAux (..), getLocalTxs,
                                                 getMemPool, mpAddrHistories,
                                                 mpLocalTxs, mpLocalTxsExtra,
                                                 topsortTxs, txOutValue,
                                                 _txOutputs)
import           Pos.Types                      (Address (..), HeaderHash,
                                                 MainBlock, Timestamp, blockTxs,
                                                 difficultyL, gbHeader,
                                                 gbhConsensus, mcdSlot, mkCoin,
                                                 prevBlockL, sumCoins,
                                                 unsafeIntegerToCoin,
                                                 unsafeSubCoin)
import           Pos.Types.Explorer             (AddrHistory, TxExtra (..))
import           Pos.Util                       (NewestFirst (..), maybeThrow)
import qualified Pos.Util.Modifier              as MM
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..),
                                                 CAddressSummary (..),
                                                 CBlockEntry (..),
                                                 CBlockSummary (..), CHash,
                                                 CHashSearchResult (..),
                                                 CSearchId (..), CTxEntry (..),
                                                 CTxId (..), CTxSummary (..),
                                                 TxInternal (..),
                                                 convertTxOutputs, fromCAddress,
                                                 fromCHash',
                                                 fromCSearchIdAddress,
                                                 fromCSearchIdHash,
                                                 fromCSearchIdTx, fromCTxId,
                                                 toBlockEntry, toBlockSummary,
                                                 toPosixTime, toTxBrief,
                                                 toTxEntry)
import           Pos.Explorer.Web.Error         (ExplorerError (..))



----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type ExplorerMode m = WorkMode SscGodTossing m

explorerServeImpl :: ExplorerMode m => m Application -> Word16 -> m ()
explorerServeImpl = flip serveImpl "*"

explorerApp :: ExplorerMode m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode m => SendActions m -> ServerT ExplorerApi m
explorerHandlers _sendActions =
      apiBlocksLast
    :<|>
      apiBlocksSummary
    :<|>
      apiBlocksTxs
    :<|>
      apiTxsLast
    :<|>
      apiTxsSummary
    :<|>
      apiAddressSummary
    :<|>
      apiSearch
  where
    apiBlocksLast     = getLastBlocksDefault
    apiBlocksSummary  = catchExplorerError . getBlockSummary
    apiBlocksTxs      = getBlockTxsDefault
    apiTxsLast        = getLastTxsDefault
    apiTxsSummary     = catchExplorerError . getTxSummary
    apiAddressSummary = catchExplorerError . getAddressSummary
    apiSearch         = catchExplorerError . searchHash

    catchExplorerError = try

    getLastBlocksDefault      limit skip =
      catchExplorerError $ getLastBlocks (defaultLimit limit) (defaultSkip skip)

    getBlockTxsDefault hash'  limit skip =
      catchExplorerError $ getBlockTxs hash' (defaultLimit limit) (defaultSkip skip)

    getLastTxsDefault         limit skip =
      catchExplorerError $ getLastTxs (defaultLimit limit) (defaultSkip skip)

    defaultLimit limit = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip  skip  = (fromIntegral $ fromMaybe 0 skip)

searchHash :: forall m. ExplorerMode m => CSearchId -> m CHashSearchResult
searchHash shash = getResult $ do
    void $ grab $ TransactionFound <$> findTx
    void $ grab $ BlockFound       <$> findBlock
    void $ grab $ AddressFound     <$> findAddress
  where
    grab :: MonadCatch m => m CHashSearchResult -> EitherT CHashSearchResult m ExplorerError
    grab = EitherT . fmap swapEither . try

    getResult :: EitherT CHashSearchResult m a -> m CHashSearchResult
    getResult action = do
      result <- runEitherT action
      case result of
        Left found -> return found
        Right _    -> throwM $ Internal "Search failed. No transactions, blocks or adresses found."

    findTx      =  getTxSummary $ fromCSearchIdTx shash
    findBlock   =  getBlockSummary $ fromCSearchIdHash shash
    findAddress =  getAddressSummary $ fromCSearchIdAddress shash

getLastBlocks :: ExplorerMode m => Word -> Word -> m [CBlockEntry]
getLastBlocks lim off = do
    tip <- GS.getTip
    let getNextBlk h _ = fmap (view prevBlockL) $
            DB.getBlockHeader @SscGodTossing h >>=
            maybeThrow (Internal "Block database is malformed!")
    start <- foldlM getNextBlk tip [0..off]

    let unfolder n h = do
            when (n == 0) $
                fail "limit!"
            MaybeT (DB.getBlock @SscGodTossing h) >>= \case
                Left gb -> unfolder n (gb ^. prevBlockL)
                Right mb -> (,) <$> lift (toBlockEntry mb) <*>
                            pure (n - 1, mb ^. prevBlockL)
    flip unfoldrM (lim, start) $ \(n, h) -> runMaybeT $ unfolder n h

getLastTxs :: ExplorerMode m => Word -> Word -> m [CTxEntry]
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) = do
    mempoolTxs <- getMempoolTxs

    let lenTxs = length mempoolTxs
        (newOff, newLim) = recalculateOffLim off lim lenTxs
        localTxsWithTs = take lim $ drop off mempoolTxs

    blockTxsWithTs <- getBlockchainTxs newOff newLim

    pure $ [toTxEntry (tiTimestamp txi) (tiTx txi) | txi <- localTxsWithTs <> blockTxsWithTs]

getBlockSummary :: ExplorerMode m => CHash -> m CBlockSummary
getBlockSummary (fromCHash' -> h) = do
    mainBlock <- getMainBlock h
    toBlockSummary mainBlock

getBlockTxs :: ExplorerMode m => CHash -> Word -> Word -> m [CTxEntry]
getBlockTxs (fromCHash' -> h) (fromIntegral -> lim) (fromIntegral -> off) = do
    blk <- getMainBlock h
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. blockTxs
    forM (take lim . drop off $ txs) $ \tx -> do
        TxExtra {..} <- GS.getTxExtra (hash tx) >>=
            maybeThrow (Internal "In-block transaction doesn't \
                                 \have extra info in DB")
        pure $ toTxEntry teReceivedTime tx

getAddressSummary :: ExplorerMode m => CAddress -> m CAddressSummary
getAddressSummary cAddr = cAddrToAddr cAddr >>= \addr -> case addr of
    PubKeyAddress sid _ -> do
        balance <- fromMaybe (mkCoin 0) <$> GS.getFtsStake sid
        -- TODO: add number of coins when it's implemented
        -- TODO: retrieve transactions from something like an index
        txIds <- getNewestFirst <$> getAddrHistory addr
        transactions <- forM txIds $ \id -> do
            extra <- getTxExtraOrFail id
            tx <- getTxMain id extra
            let txInt = TxInternal (teReceivedTime extra) tx
            pure $ toTxBrief txInt extra
        return $ CAddressSummary cAddr 0 balance transactions
    _ -> throwM $
         Internal "Non-P2PKH addresses are not supported in Explorer yet"

getTxSummary :: ExplorerMode m => CTxId -> m CTxSummary
getTxSummary cTxId = do
    -- There are two places whence we can fetch a transaction: MemPool and DB.
    -- However, TxExtra should be added in the DB when a transaction is added
    -- to MemPool. So we start with TxExtra and then figure out whence to fetch
    -- the rest.
    txId <- cTxIdToTxId cTxId
    txExtra <- getTxExtraOrFail txId

    let blockchainPlace = teBlockchainPlace txExtra
        inputOutputs = map toaOut $ NE.toList $ teInputOutputs txExtra
        receivedTime = teReceivedTime txExtra

    (ctsBlockTimeIssued, ctsBlockHeight, ctsOutputs) <-
        case blockchainPlace of
            Nothing -> do
                -- Fetching transaction from MemPool.
                tx <- fetchTxFromMempoolOrFail txId
                let txOutputs = convertTxOutputs . NE.toList . _txOutputs $
                        view _1 tx
                pure (Nothing, Nothing, txOutputs)
            Just (headerHash, txIndexInBlock) -> do
                -- Fetching transaction from DB.
                mb <- getMainBlock headerHash
                blkSlotStart <- getBlkSlotStart mb
                let blockHeight = fromIntegral $ mb ^. difficultyL
                tx <- maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
                      atMay (toList $ mb ^. blockTxs) (fromIntegral txIndexInBlock)
                let txOutputs = convertTxOutputs . NE.toList $ _txOutputs tx
                    ts = toPosixTime <$> blkSlotStart
                pure (ts, Just blockHeight, txOutputs)

    let ctsId = cTxId
        ctsTxTimeIssued = toPosixTime receivedTime
        ctsRelayedBy = Nothing
        ctsTotalInput = unsafeIntegerToCoin $ sumCoins $ map txOutValue inputOutputs
        ctsInputs = convertTxOutputs inputOutputs
        ctsTotalOutput = unsafeIntegerToCoin $ sumCoins $ map snd ctsOutputs

    when (ctsTotalOutput > ctsTotalInput) $
        throwM $ Internal "Detected tx with output greater than input"

    let ctsFees = unsafeSubCoin ctsTotalInput ctsTotalOutput
    pure $ CTxSummary {..}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

fetchTxFromMempoolOrFail :: ExplorerMode m => TxId -> m TxAux
fetchTxFromMempoolOrFail txId =
    maybeThrow (Internal "Transaction not found in the mempool") =<<
    view (mpLocalTxs . at txId) <$> getMemPool

getMempoolTxs :: ExplorerMode m => m [TxInternal]
getMempoolTxs = do
    let mkWhTx (txid, (tx, _, _)) = WithHash tx txid
    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< getLocalTxs
    localTxExtras <- MM.insertionsMap . view mpLocalTxsExtra <$> getMemPool

    pure . flip mapMaybe localTxs $ \(id, (tx, _, _)) -> do
        TxExtra {..} <- HM.lookup id localTxExtras
        pure $ TxInternal teReceivedTime tx

recalculateOffLim :: Int -> Int -> Int -> (Int, Int)
recalculateOffLim off lim lenTxs =
    if lenTxs <= off
    then (off - lenTxs, lim)
    else (0, lim - (lenTxs - off))

getBlockchainTxs :: ExplorerMode m => Int -> Int -> m [TxInternal]
getBlockchainTxs origOff origLim = do
    let unfolder off lim h = do
            when (lim <= 0) $
                fail "Finished"
            MaybeT (DB.getBlock @SscGodTossing h) >>= \case
                Left gb -> unfolder off lim (gb ^. prevBlockL)
                Right mb -> do
                    let mTxs = mb ^. blockTxs
                        lenTxs = length mTxs
                    if off >= lenTxs
                        then return ([], (off - lenTxs, lim, mb ^. prevBlockL))
                        else do
                        txs <- topsortTxsOrFail identity $ map withHash $ toList mTxs
                        let neededTxs = take lim $ drop off $ reverse txs
                            (newOff, newLim) = recalculateOffLim off lim lenTxs
                        blkTxEntries <- lift $ forM neededTxs $ \(WithHash tx id) -> do
                            TxExtra {..} <- maybeThrow (Internal "No extra info for tx in DB") =<<
                                            GS.getTxExtra id
                            pure $ TxInternal teReceivedTime tx
                        return (blkTxEntries, (newOff, newLim, mb ^. prevBlockL))

    tip <- GS.getTip
    fmap concat $ flip unfoldrM (origOff, origLim, tip) $
        \(o, l, h) -> runMaybeT $ unfolder o l h

getBlkSlotStart :: MonadSlots m => MainBlock ssc -> m (Maybe Timestamp)
getBlkSlotStart blk = getSlotStart $ blk ^. gbHeader . gbhConsensus . mcdSlot

topsortTxsOrFail :: MonadThrow m => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr =
    fromCAddress cAddr &
    either (const $ throwM $ Internal "Invalid address!") pure

cTxIdToTxId :: MonadThrow m => CTxId -> m TxId
cTxIdToTxId cTxId =
    fromCTxId cTxId &
    either (const $ throwM $ Internal "Invalid transaction id!") pure

getMainBlock :: ExplorerMode m => HeaderHash -> m (MainBlock SscGodTossing)
getMainBlock h =
    DB.getBlock h >>=
    maybeThrow (Internal "No block found") >>=
    either (const $ throwM $ Internal "Block is genesis block") pure

getTxExtra :: ExplorerMode m => TxId -> m (Maybe TxExtra)
getTxExtra id =
    MM.lookupM GS.getTxExtra id =<<
    view mpLocalTxsExtra <$> getMemPool

getTxExtraOrFail :: ExplorerMode m => TxId -> m TxExtra
getTxExtraOrFail id =
    maybeThrow (Internal "Transaction not found") =<< getTxExtra id

getAddrHistory :: ExplorerMode m => Address -> m AddrHistory
getAddrHistory addr = getMemPool >>=
    maybe (GS.getAddrHistory addr) pure . view (mpAddrHistories . at addr)

getTxMain :: ExplorerMode m => TxId -> TxExtra -> m Tx
getTxMain id TxExtra {..} = case teBlockchainPlace of
    Nothing -> view _1 <$> fetchTxFromMempoolOrFail id
    Just (hh, idx) -> do
        mb <- getMainBlock hh
        maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
            atMay (toList $ mb ^. blockTxs) $ fromIntegral idx
