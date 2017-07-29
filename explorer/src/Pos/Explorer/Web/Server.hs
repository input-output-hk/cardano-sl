{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

-- API server logic

module Pos.Explorer.Web.Server
       ( ExplorerMode
       , explorerServeImpl
       , explorerApp
       , explorerHandlers

       -- function useful for socket-io server
       , topsortTxsOrFail
       , getMempoolTxs
       , getBlocksLastPage
       ) where

import           Universum

import           Control.Lens                   (at)
import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe)
import           Data.Time.Clock.POSIX          (POSIXTime)
import           Formatting                     (build, sformat, (%))
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), hash, withHash)

import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.DB                      as DB
import qualified Pos.DB.GState                  as GS

import           Pos.Block.Core                 (Block, MainBlock,
                                                 mainBlockSlot,
                                                 mainBlockTxPayload, mcdSlot)
import           Pos.DB.Class                   (MonadDBRead)
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), TxAux, TxId,
                                                 TxOutAux (..), getLocalTxs,
                                                 getMemPool, mpLocalTxs, taTx,
                                                 topsortTxs, txOutValue,
                                                 _txOutputs)
import           Pos.Txp                        (MonadTxpMem, txpTxs)
import           Pos.Types                      (Address (..), Coin, EpochIndex,
                                                 HeaderHash, Timestamp,
                                                 difficultyL, gbHeader,
                                                 gbhConsensus,
                                                 getChainDifficulty, mkCoin,
                                                 prevBlockL, siEpoch, siSlot,
                                                 sumCoins, unsafeIntegerToCoin,
                                                 unsafeSubCoin)
import           Pos.Util                       (maybeThrow)
import           Pos.Util.Chrono                (NewestFirst (..))
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer                   (TxExtra (..), getEpochBlocks,
                                                 getPageBlocks, getTxExtra)
import qualified Pos.Explorer                   as EX (getAddrBalance,
                                                       getAddrHistory,
                                                       getTxExtra)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..),
                                                 CAddressSummary (..),
                                                 CAddressType (..),
                                                 CBlockEntry (..),
                                                 CBlockSummary (..), CHash,
                                                 CTxBrief (..), CTxEntry (..),
                                                 CTxId (..), CTxSummary (..),
                                                 TxInternal (..),
                                                 convertTxOutputs, fromCAddress,
                                                 fromCHash, fromCTxId,
                                                 getEpochIndex, getSlotIndex,
                                                 mkCCoin, tiToTxEntry,
                                                 toBlockEntry, toBlockSummary,
                                                 toCHash, toPosixTime,
                                                 toTxBrief)
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
      apiBlocksPages
    :<|>
      apiBlocksPagesTotal
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
    apiBlocksPages       = getBlocksPagesDefault
    apiBlocksPagesTotal  = getBlocksPagesTotalDefault
    apiBlocksSummary     = catchExplorerError . getBlockSummary
    apiBlocksTxs         = getBlockTxsDefault
    apiTxsLast           = getLastTxsDefault
    apiTxsSummary        = catchExplorerError . getTxSummary
    apiAddressSummary    = catchExplorerError . getAddressSummary
    apiEpochSlotSearch   = tryEpochSlotSearch

    catchExplorerError   = try

    getBlocksPagesDefault     page size  =
      catchExplorerError $ getBlocksPage page (defaultPageSize size)

    getBlocksPagesTotalDefault     size  =
      catchExplorerError $ getBlocksPagesTotal (defaultPageSize size)

    getBlockTxsDefault hash'  limit skip =
      catchExplorerError $ getBlockTxs hash' (defaultLimit limit) (defaultSkip skip)

    getLastTxsDefault         limit skip =
      catchExplorerError $ getLastTxs (defaultLimit limit) (defaultSkip skip)

    tryEpochSlotSearch   epoch maybeSlot =
      catchExplorerError $ epochSlotSearch epoch maybeSlot

    defaultPageSize size = (fromIntegral $ fromMaybe 10  size)
    defaultLimit limit   = (fromIntegral $ fromMaybe 10  limit)
    defaultSkip  skip    = (fromIntegral $ fromMaybe 0   skip)

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: (ExplorerMode m)
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- DB.getTipBlock @(Block SscGodTossing)

    pure $ maxBlocks tipBlock
  where
    maxBlocks tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL


-- | Get last blocks with a page parameter. This enables easier paging on the
-- client side and should enable a simple and thin client logic.
-- Currently the pages are in chronological order.
getBlocksPage
    :: (ExplorerMode m)
    => Maybe Word
    -> Word
    -> m (Integer, [CBlockEntry])
getBlocksPage mPageNumber pageSize = do

    -- Get total pages from the blocks.
    totalPages <- getBlocksPagesTotal pageSize

    -- Initially set on the last page number if page number not defined.
    let pageNumber = fromMaybe totalPages $ toInteger <$> mPageNumber

    -- Make sure the parameters are valid.
    when (pageNumber <= 0) $
        throwM $ Internal "Number of pages must be greater than 0."

    when (pageNumber > totalPages) $
        throwM $ Internal "Number of pages exceeds total pages number."

    -- TODO: Fix in the future.
    when (pageSize /= 10) $
        throwM $ Internal "We currently support only page size of 10."

    when (pageSize > 1000) $
        throwM $ Internal "The upper bound for pageSize is 1000."

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    pageBlocksHH    <- getPageHHsOrThrow $ fromIntegral pageNumber
    blocks          <- forM pageBlocksHH getBlockOrThrow
    cBlocksEntry    <- forM (rights blocks) toBlockEntry

    -- Return total pages and the blocks. We start from page 1.
    pure (totalPages, cBlocksEntry)
  where

    -- Either get the @HeaderHash@es from the @Page@ or throw an exception.
    getPageHHsOrThrow
        :: (DB.MonadBlockDB SscGodTossing m, MonadThrow m)
        => Int
        -> m [HeaderHash]
    getPageHHsOrThrow pageNumber = getPageBlocks pageNumber >>=
        maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on page "%build%" found!") pageNumber

    -- Either get the block from the @HeaderHash@ or throw an exception.
    getBlockOrThrow
        :: (DB.MonadBlockDB SscGodTossing m, MonadThrow m)
        => HeaderHash
        -> m (Block SscGodTossing)
    getBlockOrThrow headerHash = DB.blkGetBlock headerHash >>=
        maybeThrow (Internal "Block with hash cannot be found!")


-- | Get total pages from blocks. Calculated from
-- pageSize we pass to it.
getBlocksPagesTotal
    :: (ExplorerMode m)
    => Word
    -> m Integer
getBlocksPagesTotal pageSize = do
    -- Get total blocks in the blockchain.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Get total pages from the blocks. And we want the page
    -- with the example, the page size 10,
    -- to start with 10 + 1 == 11, not with 10 since with
    -- 10 we'll have an empty page.
    let totalPages = (blocksTotal - 1) `div` pageSizeInt

    -- We start from page 1.
    pure (totalPages + 1)
  where
    pageSizeInt     = toInteger pageSize


-- | Get the last page from the blockchain. We use the default 10
-- for the page size since this is called from __explorer only__.
getBlocksLastPage
    :: (ExplorerMode m)
    => m (Integer, [CBlockEntry])
getBlocksLastPage = getBlocksPage Nothing pageSize
  where
    pageSize :: Word
    pageSize = 10


-- | Get last transactions from the blockchain
getLastTxs
    :: ExplorerMode m
    => Word
    -> Word
    -> m [CTxEntry]
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) = do
    mempoolTxs <- getMempoolTxs

    let lenTxs = length mempoolTxs
        (newOff, newLim) = recalculateOffLim off lim lenTxs
        localTxsWithTs = take lim $ drop off mempoolTxs

    blockTxsWithTs <- getBlockchainTxs newOff newLim

    pure $ tiToTxEntry <$> localTxsWithTs <> blockTxsWithTs


-- | Get block summary
getBlockSummary
    :: ExplorerMode m
    => CHash
    -> m CBlockSummary
getBlockSummary cHash = do
    h <- unwrapOrThrow $ fromCHash cHash
    mainBlock <- getMainBlock h
    toBlockSummary mainBlock


-- | Get transactions from a block.
getBlockTxs
    :: ExplorerMode m
    => CHash
    -> Word
    -> Word
    -> m [CTxBrief]
getBlockTxs cHash (fromIntegral -> lim) (fromIntegral -> off) = do
    h <- unwrapOrThrow $ fromCHash cHash
    blk <- getMainBlock h
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. mainBlockTxPayload . txpTxs
    forM (take lim . drop off $ txs) $ \tx -> do
        extra <- EX.getTxExtra (hash tx) >>=
                 maybeThrow (Internal "In-block transaction doesn't \
                                      \have extra info in DB")
        pure $ makeTxBrief tx extra


-- | Get address summary. Can return several addresses.
-- @PubKeyAddress@, @ScriptAddress@, @RedeemAddress@ and finally
-- @UnknownAddressType@.
getAddressSummary
    :: ExplorerMode m
    => CAddress
    -> m CAddressSummary
getAddressSummary cAddr = do
    addr <- cAddrToAddr cAddr

    when (isAddressUnknown addr) $
        throwM $ Internal "Unknown address type"

    balance <- mkCCoin . fromMaybe (mkCoin 0) <$> EX.getAddrBalance addr
    txIds <- getNewestFirst <$> EX.getAddrHistory addr
    transactions <- forM txIds $ \id -> do
        extra <- getTxExtraOrFail id
        tx <- getTxMain id extra
        pure $ makeTxBrief tx extra
    pure CAddressSummary {
        caAddress = cAddr,
        caType = getAddressType addr,
        caTxNum = fromIntegral $ length transactions,
        caBalance = balance,
        caTxList = transactions
    }
  where
    isAddressUnknown = \case
        UnknownAddressType _ _ -> True
        _ -> False
    getAddressType :: Address -> CAddressType
    getAddressType = \case
        PubKeyAddress _ _ -> CPubKeyAddress
        ScriptAddress _ -> CScriptAddress
        RedeemAddress _ -> CRedeemAddress
        UnknownAddressType _ _ -> CUnknownAddress


-- Data type using in @getTxSummary@.
data BlockFields = BlockFields
    { bfTimeIssued :: !(Maybe POSIXTime)
    , bfHeight     :: !(Maybe Word)
    , bfEpoch      :: !(Maybe Word64)
    , bfSlot       :: !(Maybe Word16)
    , bfHash       :: !(Maybe CHash)
    , bfOutputs    :: ![(CAddress, Coin)]
    }


-- | Get transaction summary from transaction id. Looks at both the database
-- and the memory (mempool) for the transaction. What we have at the mempool
-- are transactions that have to be written in the blockchain.
getTxSummary
    :: (ExplorerMode m)
    => CTxId
    -> m CTxSummary
getTxSummary cTxId = do
    -- There are two places whence we can fetch a transaction: MemPool and DB.
    -- However, TxExtra should be added in the DB when a transaction is added
    -- to MemPool. So we start with TxExtra and then figure out whence to fetch
    -- the rest.
    txId                    <- cTxIdToTxId cTxId
    -- Get from database, txExtra
    txExtra                 <- getTxExtraOrFail txId

    -- Return transaction extra (txExtra) fields
    let blockchainPlace     = teBlockchainPlace txExtra
        inputOutputs        = map toaOut $ NE.toList $ teInputOutputs txExtra
        receivedTime        = teReceivedTime txExtra

    -- fetch block fields (DB or mempool)
    blockFields <- fetchBlockFields txId blockchainPlace

    let ctsBlockTimeIssued  = bfTimeIssued blockFields
        ctsBlockHeight      = bfHeight blockFields
        ctsBlockEpoch       = bfEpoch blockFields
        ctsBlockSlot        = bfSlot blockFields
        ctsBlockHash        = bfHash blockFields
        outputs             = bfOutputs blockFields

        ctsId               = cTxId
        ctsOutputs          = map (second mkCCoin) outputs
        ctsTxTimeIssued     = toPosixTime receivedTime
        ctsRelayedBy        = Nothing
        ctsTotalInput       = mkCCoin totalInput
        totalInput          = unsafeIntegerToCoin $ sumCoins $ map txOutValue inputOutputs
        ctsInputs           = map (second mkCCoin) $ convertTxOutputs inputOutputs
        ctsTotalOutput      = mkCCoin totalOutput
        totalOutput         = unsafeIntegerToCoin $ sumCoins $ map snd outputs

    -- Verify that strange things don't happen with transactions
    when (totalOutput > totalInput) $
        throwM $ Internal "Detected tx with output greater than input"

    let ctsFees = mkCCoin $ unsafeSubCoin totalInput totalOutput
    pure $ CTxSummary {..}
      where

        -- General fetch that fetches either from DB or mempool
        fetchBlockFields txId Nothing =
            fetchBlockFieldsFromMempool txId

        fetchBlockFields _    (Just (headerHash, txIndexInBlock)) =
            fetchBlockFieldsFromDb headerHash txIndexInBlock

        -- Fetching transaction from MemPool.
        fetchBlockFieldsFromMempool txId = do
            tx              <- fetchTxFromMempoolOrFail txId

            let txOutputs   = convertTxOutputs . NE.toList . _txOutputs $ taTx tx
            pure BlockFields
                    { bfTimeIssued = Nothing
                    , bfHeight = Nothing
                    , bfEpoch = Nothing
                    , bfSlot = Nothing
                    , bfHash = Nothing
                    , bfOutputs = txOutputs
                    }

        -- Fetching transaction from DB.
        fetchBlockFieldsFromDb headerHash txIndexInBlock =  do

            mb                <- getMainBlock headerHash
            blkSlotStart      <- getBlkSlotStart mb

            let blockHeight   = fromIntegral $ mb ^. difficultyL

            -- Get block epoch and slot index
            let blkHeaderSlot = mb ^. mainBlockSlot
                epochIndex    = getEpochIndex $ siEpoch blkHeaderSlot
                slotIndex     = getSlotIndex  $ siSlot  blkHeaderSlot
                blkHash       = toCHash headerHash

            tx <- maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
                  atMay (toList $ mb ^. mainBlockTxPayload . txpTxs) (fromIntegral txIndexInBlock)

            let txOutputs     = convertTxOutputs . NE.toList $ _txOutputs tx
                ts            = toPosixTime <$> blkSlotStart
            pure BlockFields
                    { bfTimeIssued = ts
                    , bfHeight = Just blockHeight
                    , bfEpoch = Just epochIndex
                    , bfSlot = Just slotIndex
                    , bfHash = Just blkHash
                    , bfOutputs = txOutputs
                    }

-- | Search the blocks by epoch and slot. Slot is optional.
epochSlotSearch
    :: (ExplorerMode m)
    => EpochIndex
    -> Maybe Word16
    -> m [CBlockEntry]
epochSlotSearch epochIndex slotIndex = do

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    epochBlocksHH   <- getPageHHsOrThrow epochIndex
    blocks          <- forM epochBlocksHH getBlockOrThrow
    cBlocksEntry    <- forM (getEpochSlots slotIndex (rights blocks)) toBlockEntry

    pure cBlocksEntry
  where
    -- Get epoch slot block that's being searched or return all epochs if
    -- the slot is @Nothing@.
    getEpochSlots 
        :: Maybe Word16 
        -> [MainBlock SscGodTossing] 
        -> [MainBlock SscGodTossing]
    getEpochSlots Nothing          blocks = blocks
    getEpochSlots (Just slotIndex') blocks = filter filterBlocksBySlotIndex blocks
      where
        getBlockSlotIndex
            :: MainBlock SscGodTossing
            -> Word16
        getBlockSlotIndex block = getSlotIndex $ siSlot $ block ^. mainBlockSlot

        filterBlocksBySlotIndex
            :: MainBlock SscGodTossing
            -> Bool
        filterBlocksBySlotIndex block = getBlockSlotIndex block == slotIndex'

    -- Either get the @HeaderHash@es from the @Epoch@ or throw an exception.
    getPageHHsOrThrow 
        :: (DB.MonadBlockDB SscGodTossing m, MonadThrow m)
        => EpochIndex
        -> m [HeaderHash]
    getPageHHsOrThrow epoch = getEpochBlocks epoch >>=
        maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on epoch "%build%" found!") epoch

    -- Either get the block from the @HeaderHash@ or throw an exception.
    getBlockOrThrow 
        :: (DB.MonadBlockDB SscGodTossing m, MonadThrow m) 
        => HeaderHash 
        -> m (Block SscGodTossing)
    getBlockOrThrow headerHash = DB.blkGetBlock headerHash >>= 
        maybeThrow (Internal "Block with hash cannot be found!")


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


makeTxBrief :: Tx -> TxExtra -> CTxBrief
makeTxBrief tx extra = toTxBrief (TxInternal extra tx)

unwrapOrThrow :: ExplorerMode m => Either Text a -> m a
unwrapOrThrow = either (throwM . Internal) pure

-- | Get transaction from memory (STM) or throw exception.
fetchTxFromMempoolOrFail :: ExplorerMode m => TxId -> m TxAux
fetchTxFromMempoolOrFail txId =
    view (mpLocalTxs . at txId) <$> getMemPool >>=
    maybeThrow (Internal "Transaction not found in the mempool")

getMempoolTxs :: ExplorerMode m => m [TxInternal]
getMempoolTxs = do

    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< tlocalTxs

    fmap catMaybes . forM localTxs $ \(id, txAux) -> do
        mextra <- getTxExtra id
        forM mextra $ \extra -> pure $ TxInternal extra (taTx txAux)
  where
    tlocalTxs :: (MonadIO m, MonadTxpMem e m) => m [(TxId, TxAux)]
    tlocalTxs = getLocalTxs

    mkWhTx :: (TxId, TxAux) -> WithHash Tx
    mkWhTx (txid, txAux) = WithHash (taTx txAux) txid

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
            MaybeT (DB.blkGetBlock @SscGodTossing h) >>= \case
                Left gb -> unfolder off lim (gb ^. prevBlockL)
                Right mb -> do
                    let mTxs   = mb ^. mainBlockTxPayload . txpTxs
                        lenTxs = length mTxs
                    if off >= lenTxs
                        then return ([], (off - lenTxs, lim, mb ^. prevBlockL))
                        else do
                        txs <- topsortTxsOrFail identity $ map withHash $ toList mTxs
                        let neededTxs = take lim $ drop off $ reverse txs
                            (newOff, newLim) = recalculateOffLim off lim lenTxs
                        blkTxEntries <- lift $ forM neededTxs $ \(WithHash tx id) -> do
                            extra <- maybeThrow (Internal "No extra info for tx in DB") =<<
                                            EX.getTxExtra id
                            pure $ TxInternal extra tx
                        return (blkTxEntries, (newOff, newLim, mb ^. prevBlockL))

    tip <- GS.getTip
    fmap concat $ flip unfoldrM (origOff, origLim, tip) $
        \(o, l, h) -> runMaybeT $ unfolder o l h

getBlkSlotStart :: MonadSlots m => MainBlock ssc -> m (Maybe Timestamp)
getBlkSlotStart blk = getSlotStart $ blk ^. gbHeader . gbhConsensus . mcdSlot

topsortTxsOrFail :: (MonadThrow m, Eq a) => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

-- | Convert server address to client address,
-- with the possible exception (effect).
cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr = either exception pure (fromCAddress cAddr)
  where
    exception = const $ throwM $ Internal "Invalid address!"

-- | Convert server transaction to client transaction,
-- with the possible exception (effect).
cTxIdToTxId :: MonadThrow m => CTxId -> m TxId
cTxIdToTxId cTxId = either exception pure (fromCTxId cTxId)
  where
    exception = const $ throwM $ Internal "Invalid transaction id!"

getMainBlock :: ExplorerMode m => HeaderHash -> m (MainBlock SscGodTossing)
getMainBlock h =
    DB.blkGetBlock h >>=
    maybeThrow (Internal "No block found") >>=
    either (const $ throwM $ Internal "Block is genesis block") pure

-- | Get transaction extra from the database, and if you don't find it
-- throw an exception.
getTxExtraOrFail :: MonadDBRead m => TxId -> m TxExtra
getTxExtraOrFail txId = getTxExtra txId >>= maybeThrow exception
  where
    exception = Internal "Transaction not found"

getTxMain :: ExplorerMode m => TxId -> TxExtra -> m Tx
getTxMain id TxExtra {..} = case teBlockchainPlace of
    Nothing -> taTx <$> fetchTxFromMempoolOrFail id
    Just (hh, idx) -> do
        mb <- getMainBlock hh
        maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
            atMay (toList $ mb ^. mainBlockTxPayload . txpTxs) $ fromIntegral idx
