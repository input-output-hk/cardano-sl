{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
-- API server logic

module Pos.Explorer.Web.Server
       ( ExplorerMode
       , explorerServeImpl
       , explorerApp
       , explorerHandlers
       , topsortTxsOrFail
       , getMempoolTxs
       ) where

import           Control.Lens                   (at)
import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
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
import qualified Pos.DB.GState.Balances         as GS

import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class                  (SscHelpersClass)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), TxAux, TxId, TxOutAux (..),
                                                 getLocalTxs, getMemPool, mpLocalTxs,
                                                 topsortTxs, txOutValue, _txOutputs)
import           Pos.Types                      (Address (..), Block, EpochIndex,
                                                 HeaderHash, LocalSlotIndex (..),
                                                 MainBlock, Timestamp, blockSlot,
                                                 blockTxs, difficultyL, gbHeader,
                                                 gbhConsensus, genesisHash, mcdSlot,
                                                 mkCoin, prevBlockL, siEpoch, siSlot,
                                                 sumCoins, unsafeIntegerToCoin,
                                                 unsafeSubCoin)
import           Pos.Util                       (maybeThrow)
import           Pos.Util.Chrono                (NewestFirst (..))
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer                   (TxExtra (..), getTxExtra)
import qualified Pos.Explorer                   as EX (getAddrHistory, getTxExtra)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CBlockEntry (..), CBlockSummary (..),
                                                 CHash, CTxBrief (..), CTxEntry (..),
                                                 CTxId (..), CTxSummary (..),
                                                 TxInternal (..), convertTxOutputs,
                                                 fromCAddress, fromCHash, fromCTxId,
                                                 tiToTxEntry, toBlockEntry,
                                                 toBlockSummary, toPosixTime, toTxBrief)
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
      apiEpochSlotSearch
  where
    apiBlocksLast       = getLastBlocksDefault
    apiBlocksSummary    = catchExplorerError . getBlockSummary
    apiBlocksTxs        = getBlockTxsDefault
    apiTxsLast          = getLastTxsDefault
    apiTxsSummary       = catchExplorerError . getTxSummary
    apiAddressSummary   = catchExplorerError . getAddressSummary
    apiEpochSlotSearch  = tryEpochSlotSearch

    catchExplorerError = try

    getLastBlocksDefault      limit skip =
      catchExplorerError $ getLastBlocks (defaultLimit limit) (defaultSkip skip)

    getBlockTxsDefault hash'  limit skip =
      catchExplorerError $ getBlockTxs hash' (defaultLimit limit) (defaultSkip skip)

    getLastTxsDefault         limit skip =
      catchExplorerError $ getLastTxs (defaultLimit limit) (defaultSkip skip)

    tryEpochSlotSearch   epoch maybeSlot =
      catchExplorerError $ epochSlotSearch epoch maybeSlot

    defaultLimit limit = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip  skip  = (fromIntegral $ fromMaybe 0 skip)

-- | Search the blocks by epoch and slot. Slot is optional.
epochSlotSearch
    :: (ExplorerMode m)
    => EpochIndex
    -> Maybe Word16
    -> m [CBlockEntry]
epochSlotSearch epochIndex slotIndex = do
    blocks <- findBlocksByEpoch >>= traverse toBlockEntry
    if null blocks
        then throwM $ Internal "No epoch/slots found."
        else pure blocks
  where
    findBlocksByEpoch = getBlocksByEpoch @SscGodTossing epochIndex localSlotIndex
    localSlotIndex    = fmap LocalSlotIndex slotIndex

-- | Get all blocks by epoch and slot. The slot is optional, if it exists,
-- it just adds another predicate to match it.
getBlocksByEpoch
    :: (SscHelpersClass ssc, ExplorerMode m)
    => EpochIndex
    -> Maybe LocalSlotIndex
    -> m [MainBlock ssc]
getBlocksByEpoch epochIndex mSlotIndex = do
    tipHash <- GS.getTip
    filterMainBlocks tipHash findBlocksByEpochPred
      where
        findBlocksByEpochPred mb = (siEpoch $ mb ^. blockSlot) == epochIndex &&
                fromMaybe True ((siSlot (mb ^. blockSlot) ==) <$> mSlotIndex)
--
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
            MaybeT (DB.getBlock @SscGodTossing h) >>= \mBlock -> case mBlock of
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

    pure $ tiToTxEntry <$> localTxsWithTs <> blockTxsWithTs

getBlockSummary :: ExplorerMode m => CHash -> m CBlockSummary
getBlockSummary cHash = do
    h <- unwrapOrThrow $ fromCHash cHash
    mainBlock <- getMainBlock h
    toBlockSummary mainBlock

getBlockTxs :: ExplorerMode m => CHash -> Word -> Word -> m [CTxBrief]
getBlockTxs cHash (fromIntegral -> lim) (fromIntegral -> off) = do
    h <- unwrapOrThrow $ fromCHash cHash
    blk <- getMainBlock h
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. blockTxs
    forM (take lim . drop off $ txs) $ \tx -> do
        extra <- EX.getTxExtra (hash tx) >>=
                 maybeThrow (Internal "In-block transaction doesn't \
                                      \have extra info in DB")
        pure $ makeTxBrief tx extra

getAddressSummary :: ExplorerMode m => CAddress -> m CAddressSummary
getAddressSummary cAddr = cAddrToAddr cAddr >>= \addr -> case addr of
    PubKeyAddress sid _ -> do
        balance <- fromMaybe (mkCoin 0) <$> GS.getRealStake sid
        -- TODO: add number of coins when it's implemented
        -- TODO: retrieve transactions from something like an index
        txIds <- getNewestFirst <$> EX.getAddrHistory addr
        transactions <- forM txIds $ \id -> do
            extra <- getTxExtraOrFail id
            tx <- getTxMain id extra
            pure $ makeTxBrief tx extra
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


makeTxBrief :: Tx -> TxExtra -> CTxBrief
makeTxBrief tx extra = toTxBrief txInt extra
  where txInt = TxInternal (teReceivedTime extra) tx

-- | Find all `MainBlock` by applying the *predicate*, starting from *headerHash*
filterMainBlocks
    :: (SscHelpersClass ssc, ExplorerMode m)
    => HeaderHash
    -> (MainBlock ssc -> Bool)
    -> m [MainBlock ssc]
filterMainBlocks headerHash predicate = rights <$> generalBlockSearch
  where
    generalBlockSearch    = filterAllBlocks headerHash specializedPred (pure [])
    specializedPred block = either (const False) predicate block

-- | Find all blocks matching the sent predicate. This is a generic function
-- that can be called with either `MainBlock` or `GenesisBlock` in mind.
filterAllBlocks
    :: (SscHelpersClass ssc, ExplorerMode m)
    => HeaderHash
    -> (Block ssc -> Bool)
    -> m [Block ssc]
    -> m [Block ssc]
filterAllBlocks headerHash predicate acc
    -- When we reach the genesis block, return the accumulator. This is
    -- literaly the first block ever, so we reached the begining of the
    -- whole blockchain and there is nothing more to search.
    | headerHash == genesisHash = acc
    -- Otherwise iterate back from the top block (called tip) and add all
    -- blocks (hash) to accumulator satisfying the predicate.
    | otherwise = do
        -- Get the block with the sent hash, throw exception if/when the block
        -- search fails.
        block <- DB.getBlock headerHash >>=
            maybeThrow (Internal "Block with hash cannot be found!")
        -- If there is a block then iterate backwards with the predicate
        let prevBlock = block ^. prevBlockL

        if predicate block
            -- When the predicate is true, add the block to the list
            then filterAllBlocks prevBlock predicate ((:) <$> pure block <*> acc)
            -- When the predicate is false, don't add the block to the list
            else filterAllBlocks prevBlock predicate acc

unwrapOrThrow :: ExplorerMode m => Either Text a -> m a
unwrapOrThrow = either (throwM . Internal) pure

fetchTxFromMempoolOrFail :: ExplorerMode m => TxId -> m TxAux
fetchTxFromMempoolOrFail txId =
    maybeThrow (Internal "Transaction not found in the mempool") =<<
    view (mpLocalTxs . at txId) <$> getMemPool

getMempoolTxs :: ExplorerMode m => m [TxInternal]
getMempoolTxs = do
    let mkWhTx (txid, (tx, _, _)) = WithHash tx txid
    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< getLocalTxs

    fmap catMaybes . forM localTxs $ \(id, (tx, _, _)) -> do
        mextra <- getTxExtra id
        forM mextra $ \TxExtra {..} -> pure $ TxInternal teReceivedTime tx

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
                                            EX.getTxExtra id
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
{-}
getTxExtra :: ExplorerMode m => TxId -> m (Maybe TxExtra)
getTxExtra id =
    MM.lookupM EX.getTxExtra id =<< getTxExtra
-}
getTxExtraOrFail :: ExplorerMode m => TxId -> m TxExtra
getTxExtraOrFail id =
    maybeThrow (Internal "Transaction not found") =<< getTxExtra id

getTxMain :: ExplorerMode m => TxId -> TxExtra -> m Tx
getTxMain id TxExtra {..} = case teBlockchainPlace of
    Nothing -> view _1 <$> fetchTxFromMempoolOrFail id
    Just (hh, idx) -> do
        mb <- getMainBlock hh
        maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
            atMay (toList $ mb ^. blockTxs) $ fromIntegral idx
