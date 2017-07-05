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
       , getLastBlocks
       , getBlocksLastPage
       ) where


import           Control.Lens                   (at, _Right, _Wrapped)
import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Time.Clock.POSIX          (POSIXTime)
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)

import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), hash, withHash)

import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.DB                      as DB
import qualified Pos.DB.GState                  as GS

import           Pos.Block.Core                 (Block, MainBlock, mainBlockSlot,
                                                 mainBlockTxPayload, mcdSlot)
import           Pos.Constants                  (genesisHash)
import           Pos.DB.Class                   (MonadDBRead)
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), TxAux, TxId, TxOutAux (..),
                                                 getLocalTxs, getMemPool, mpLocalTxs,
                                                 taTx, topsortTxs, txOutValue, _txOutputs)
import           Pos.Txp                        (MonadTxpMem, txpTxs)
import           Pos.Types                      (Address (..), Coin, EpochIndex, HeaderHash,
                                                 LocalSlotIndex (..), Timestamp,
                                                 difficultyL, gbHeader, gbhConsensus,
                                                 getChainDifficulty, headerHashG, mkCoin,
                                                 mkLocalSlotIndex, prevBlockL, siEpoch,
                                                 siSlot, sumCoins, unsafeIntegerToCoin,
                                                 unsafeSubCoin)
import           Pos.Util                       (maybeThrow)
import           Pos.Util.Chrono                (NewestFirst (..))
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer                   (TxExtra (..), getTxExtra)
import qualified Pos.Explorer                   as EX (getAddrBalance, getAddrHistory,
                                                       getTxExtra)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CAddressType (..), CBlockEntry (..),
                                                 CBlockSummary (..), CHash, CTxBrief (..),
                                                 CTxEntry (..), CTxId (..),
                                                 CTxSummary (..), TxInternal (..),
                                                 convertTxOutputs, fromCAddress,
                                                 fromCHash, fromCTxId, getEpochIndex,
                                                 getSlotIndex, mkCCoin, tiToTxEntry,
                                                 toBlockEntry, toBlockSummary, toCHash,
                                                 toPosixTime, toTxBrief)
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
      apiBlocksTotal
    :<|>
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
    apiBlocksLast        = getLastBlocksDefault
    apiBlocksTotal       = catchExplorerError getBlocksTotal
    apiBlocksPages       = getBlocksPagesDefault
    apiBlocksPagesTotal  = getBlocksPagesTotalDefault
    apiBlocksSummary     = catchExplorerError . getBlockSummary
    apiBlocksTxs         = getBlockTxsDefault
    apiTxsLast           = getLastTxsDefault
    apiTxsSummary        = catchExplorerError . getTxSummary
    apiAddressSummary    = catchExplorerError . getAddressSummary
    apiEpochSlotSearch   = tryEpochSlotSearch

    catchExplorerError   = try

    getLastBlocksDefault      limit skip =
      catchExplorerError $ getLastBlocks (defaultLimit limit) (defaultSkip skip)

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

-- | Get last blocks from the blockchain.
--
-- What we see when offset < blocksTotal is:
--
--  * we start at blocksTotal - offset (we have 180 blocks and we offset them
--    by 170 - we start at the block 10)
--
--  * we end at blocksTotal - offset - limit (we have 180 blocks and we offset them
--    by 170 and set the limit to 10 - we end at the block 0)
--
-- Why this offset/limit scheme - https://www.petefreitag.com/item/451.cfm
getLastBlocks
    :: (ExplorerMode m)
    => Word
    -> Word
    -> m [CBlockEntry]
getLastBlocks limit offset = do
    -- Get tip block header hash.
    tipHash     <- GS.getTip

    -- Get total blocks in the blockchain. We presume that the chance that the
    -- a new block is going to be generated from here until we search for them
    -- is very low.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Make sure we aren't offseting more than the beginning of the blockchain.
    when (offsetInt > blocksTotal) $
        throwM $ Internal "Offset cannot be greater than total blocks number."

    -- Calculate from where to where we should take blocks.
    let blocksEndIndex   = blocksTotal - offsetInt
    let blocksStartIndex = max 0 (blocksEndIndex - limitInt)

    -- Verify limit/offset calculation.
    when (blocksStartIndex > blocksEndIndex) $
        throwM $ Internal "Starting index cannot be larger than the beginning index."

    -- Find the end main block at the end index if it exists, and return it.
    foundEndBlock <- findMainBlockWithIndex tipHash blocksEndIndex >>=
        maybeThrow (Internal "Block with specified index cannot be found!")

    -- Get the header hash from the found end block.
    let foundEndHeaderHash  = foundEndBlock ^. headerHashG

    -- Take blocks until you reach the start index.
    let takeBlocks block    = getBlockIndex block >= blocksStartIndex

    -- Now we can reuse an existing function to fetch blocks.
    foundBlocks   <- DB.loadBlocksWhile @SscGodTossing takeBlocks foundEndHeaderHash

    -- Unwrap the blocks from @NewestFirst@ wrapper.
    let blocks     = foundBlocks ^. _Wrapped
    -- We want just the Main blocks, not the Genesis blocks, so we fetch them.
    let mainBlocks = rights blocks

    -- Transfrom all @MainBlock@ to @CBlockEntry@.
    pure mainBlocks >>= traverse toBlockEntry
  where
    offsetInt           = toInteger offset
    limitInt            = toInteger limit - 1 -- Remove included block
    getBlockIndex block = toInteger $ getChainDifficulty $ block ^. difficultyL

    -- | Find block matching the sent index/difficulty.
    findMainBlockWithIndex
        :: (DB.MonadBlockDB SscGodTossing m, MonadDBRead m)
        => HeaderHash
        -> Integer
        -> m (Maybe (MainBlock SscGodTossing))
    findMainBlockWithIndex headerHash index
        -- When we reach the genesis block, return @Nothing@. This is
        -- literaly the first block ever, so we reached the begining of the
        -- whole blockchain and there is nothing more to search.
        | headerHash == genesisHash = pure $ Nothing
        -- Otherwise iterate back from the top block (called tip) and
        -- search for the block satisfying the predicate.
        | otherwise = do
            -- Get the block with the sent hash, throw exception if/when the block
            -- search fails.
            block <- DB.blkGetBlock @SscGodTossing headerHash >>=
                maybeThrow (Internal "Block with hash cannot be found!")

            -- If there is a block then iterate backwards with the predicate
            let prevBlock = block ^. prevBlockL

            -- If the index is correct and it's a @Main@ block
            if (getBlockIndex block == index) && (isRight block)
                -- When the predicate is true, return the @Main@ block
                then pure $ block ^? _Right
                -- When the predicate is false, keep searching backwards
                else findMainBlockWithIndex prevBlock index


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

    -- Initially set on the last page number.
    let pageNumber      = fromMaybe totalPages $ toInteger <$> mPageNumber
    let calculateOffset = pageNumber * pageSizeInt
    let pageNumberInt   = toInteger pageNumber

    -- Get total blocks in the blockchain.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Make sure the parameters are valid.
    when (pageNumberInt > totalPages) $
        throwM $ Internal "Number of pages exceeds total pages number."

    when (pageSize > 1000) $
        throwM $ Internal "The upper bound for pageSize is 1000."

    -- Calculate the start position. We use Integer so we don't underflow Word
    -- and go on the end.
    let startPosition     = blocksTotal - (fromIntegral calculateOffset)
    let safeStartPosition = max 0 startPosition

    -- Let's calculate the maximum number of rows seens if on last page.
    let calculateLimit    = if (pageNumberInt == totalPages) &&
                               (blocksTotal `mod` pageSizeInt /= 0)
                            then blocksTotal `mod` pageSizeInt
                            else pageSizeInt

    -- Fetch last blocks
    pageBlocks <-
        getLastBlocks (fromIntegral calculateLimit) (fromIntegral safeStartPosition)

    -- Return total pages and the blocks. We start from page 1.
    pure (totalPages, pageBlocks)
  where
    pageSizeInt     = toInteger pageSize


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
    blocks <- findBlocksByEpoch >>= traverse toBlockEntry
    if null blocks
        then throwM $ Internal "No epoch/slots found."
        else pure blocks
  where
    findBlocksByEpoch = getBlocksByEpoch epochIndex localSlotIndex
    localSlotIndex    = slotIndex >>= mkMLocalSlotIndex

    -- | Get all blocks by epoch and slot. The slot is optional, if it exists,
    -- it just adds another predicate to match it.
    getBlocksByEpoch
        :: (ExplorerMode m)
        => EpochIndex
        -> Maybe LocalSlotIndex
        -> m [MainBlock SscGodTossing]
    getBlocksByEpoch epochIndex' mSlotIndex = do
        tipHash <- GS.getTip
        filterMainBlocks tipHash findBlocksByEpochPred
          where
            findBlocksByEpochPred mb = (siEpoch $ mb ^. mainBlockSlot) == epochIndex' &&
                    fromMaybe True ((siSlot (mb ^. mainBlockSlot) ==) <$> mSlotIndex)

    -- | Find all `MainBlock` by applying the *predicate*, starting from *headerHash*
    filterMainBlocks
        :: (ExplorerMode m)
        => HeaderHash
        -> (MainBlock SscGodTossing -> Bool)
        -> m [MainBlock SscGodTossing]
    filterMainBlocks headerHash predicate = rights <$> generalBlockSearch
      where
        generalBlockSearch    = filterAllBlocks headerHash specializedPred (pure [])
        specializedPred block = either (const False) predicate block

    -- | Find all blocks matching the sent predicate. This is a generic function
    -- that can be called with either `MainBlock` or `GenesisBlock` in mind.
    filterAllBlocks
        :: (ExplorerMode m)
        => HeaderHash
        -> (Block SscGodTossing -> Bool)
        -> m [Block SscGodTossing]
        -> m [Block SscGodTossing]
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
            block <- DB.blkGetBlock headerHash >>=
                maybeThrow (Internal "Block with hash cannot be found!")
            -- If there is a block then iterate backwards with the predicate
            let prevBlock = block ^. prevBlockL

            if predicate block
                -- When the predicate is true, add the block to the list
                then filterAllBlocks prevBlock predicate ((:) <$> pure block <*> acc)
                -- When the predicate is false, don't add the block to the list
                else filterAllBlocks prevBlock predicate acc


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

-- | Utility function for instantiating @Maybe@ @LocalSlotIndex@
mkMLocalSlotIndex :: Word16 -> Maybe LocalSlotIndex
mkMLocalSlotIndex idx = do
    eLocalSlotIndex <- runExceptT $ mkLocalSlotIndex idx
    eLocalSlotIndex ^? _Right
