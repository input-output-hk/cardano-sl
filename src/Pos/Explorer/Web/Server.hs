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

       -- function useful for socket-io server
       , topsortTxsOrFail
       , getMempoolTxs
       , getLastBlocks
       ) where

import           Control.Lens                   (at, _Wrapped, _Right)
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

import           Pos.DB.Class                   (MonadDB)
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
                                                 gbhConsensus, genesisHash, headerHashG,
                                                 getChainDifficulty, mcdSlot, mkCoin,
                                                 prevBlockL, siEpoch, siSlot, sumCoins,
                                                 unsafeIntegerToCoin, unsafeSubCoin)
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
                                                 fromCHash, fromCTxId, mkCCoin,
                                                 tiToTxEntry, toBlockEntry,
                                                 toBlockSummary, toPosixTime,
                                                 toTxBrief, getEpochIndex,
                                                 getSlotIndex)
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
      catchExplorerError $ getBlocksPage (defaultPage page) (defaultPageSize size)

    getBlocksPagesTotalDefault     size  =
      catchExplorerError $ getBlocksPagesTotal (defaultPageSize size)

    getBlockTxsDefault hash'  limit skip =
      catchExplorerError $ getBlockTxs hash' (defaultLimit limit) (defaultSkip skip)

    getLastTxsDefault         limit skip =
      catchExplorerError $ getLastTxs (defaultLimit limit) (defaultSkip skip)

    tryEpochSlotSearch   epoch maybeSlot =
      catchExplorerError $ epochSlotSearch epoch maybeSlot

    defaultPage  page    = (fromIntegral $ fromMaybe 1   page)
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
    :: (MonadDB m, MonadSlots m) 
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
    limitInt            = toInteger limit
    getBlockIndex block = toInteger $ getChainDifficulty $ block ^. difficultyL

    -- | Find block matching the sent index/difficulty.
    findMainBlockWithIndex
        :: (MonadDB m)
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
            block <- DB.getBlock @SscGodTossing headerHash >>=
                maybeThrow (Internal "Block with hash cannot be found!")
            -- If there is a block then iterate backwards with the predicate
            let prevBlock = block ^. prevBlockL

            if getBlockIndex block == index
                -- When the predicate is true, return the @Main@ block
                then pure $ block ^? _Right
                -- When the predicate is false, keep searching backwards
                else findMainBlockWithIndex prevBlock index


-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: (MonadDB m)
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- DB.getTipBlock @SscGodTossing
    -- -1 is for the genesis block which isn't visible and contains no
    -- valuable information
    pure $ max 0 (maxBlocks tipBlock - 1)
  where
    maxBlocks tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL


-- | Get last blocks with a page parameter. This enables easier paging on the
-- client side and should enable a simple and thin client logic.
getBlocksPage 
    :: (MonadDB m, MonadSlots m) 
    => Word 
    -> Word 
    -> m (Integer, [CBlockEntry])
getBlocksPage pageNumber pageSize = do

    -- Get total pages from the blocks.
    totalPages <- getBlocksPagesTotal pageSize

    -- Make sure the parameters are valid.
    when ((pageNumberInt - 1) > totalPages) $
        throwM $ Internal "Number of pages exceeds total pages number."

    when (pageSize > 1000) $
        throwM $ Internal "The upper bound for pageSize is 1000."

    -- Calculate the start position
    let startPosition = calculateOffset

    -- Fetch last blocks
    pageBlocks <- getLastBlocks pageSize startPosition

    -- Return total pages and the blocks. We start from page 1.
    pure (totalPages, pageBlocks)
  where
    calculateOffset = (pageNumber - 1) * pageSize
    pageNumberInt   = toInteger pageNumber


-- | Get total pages from blocks. Calculated from 
-- pageSize we pass to it.
getBlocksPagesTotal
    :: (MonadDB m)
    => Word
    -> m Integer
getBlocksPagesTotal pageSize = do
    -- Get total blocks in the blockchain.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Get total pages from the blocks.
    let totalPages = blocksTotal `div` pageSizeInt

    -- We start from page 1.
    pure (totalPages + 1)
  where
    pageSizeInt     = toInteger pageSize


-- | Get last transactions from the blockchain
getLastTxs :: ExplorerMode m => Word -> Word -> m [CTxEntry]
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) = do
    mempoolTxs <- getMempoolTxs

    let lenTxs = length mempoolTxs
        (newOff, newLim) = recalculateOffLim off lim lenTxs
        localTxsWithTs = take lim $ drop off mempoolTxs

    blockTxsWithTs <- getBlockchainTxs newOff newLim

    pure $ tiToTxEntry <$> localTxsWithTs <> blockTxsWithTs


-- | Get block summary
getBlockSummary :: ExplorerMode m => CHash -> m CBlockSummary
getBlockSummary cHash = do
    h <- unwrapOrThrow $ fromCHash cHash
    mainBlock <- getMainBlock h
    toBlockSummary mainBlock


-- | Get transactions from a block.
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


-- | Get address summary. Can return several addresses.
-- @PubKeyAddress@, @ScriptAddress@, @RedeemAddress@ and finally
-- @UnknownAddressType@.
getAddressSummary :: ExplorerMode m => CAddress -> m CAddressSummary
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


-- | Get transaction summary from transaction id. Looks at both the database
-- and the memory (mempool) for the transaction. What we have at the mempool
-- are transactions that have to be written in the blockchain.
getTxSummary :: ExplorerMode m => CTxId -> m CTxSummary
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

    let ctsBlockTimeIssued  = blockFields ^. _1
        ctsBlockHeight      = blockFields ^. _2
        ctsBlockEpoch       = blockFields ^. _3
        ctsBlockSlot        = blockFields ^. _4
        outputs             = blockFields ^. _5

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

            let txOutputs   = convertTxOutputs . NE.toList . _txOutputs $
                    view _1 tx
            pure (Nothing, Nothing, Nothing, Nothing, txOutputs)

        -- Fetching transaction from DB.
        fetchBlockFieldsFromDb headerHash txIndexInBlock =  do

            mb                <- getMainBlock headerHash
            blkSlotStart      <- getBlkSlotStart mb

            let blockHeight   = fromIntegral $ mb ^. difficultyL

            -- Get block epoch and slot index
            let blkHeaderSlot = mb ^. blockSlot
            let epochIndex    = getEpochIndex $ siEpoch blkHeaderSlot
            let slotIndex     = getSlotIndex  $ siSlot  blkHeaderSlot

            tx <- maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
                  atMay (toList $ mb ^. blockTxs) (fromIntegral txIndexInBlock)

            let txOutputs     = convertTxOutputs . NE.toList $ _txOutputs tx
                ts            = toPosixTime <$> blkSlotStart
            pure (ts, Just blockHeight, Just epochIndex, Just slotIndex, txOutputs)


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
    getBlocksByEpoch epochIndex' mSlotIndex = do
        tipHash <- GS.getTip
        filterMainBlocks tipHash findBlocksByEpochPred
          where
            findBlocksByEpochPred mb = (siEpoch $ mb ^. blockSlot) == epochIndex' &&
                    fromMaybe True ((siSlot (mb ^. blockSlot) ==) <$> mSlotIndex)


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

-- | Get transaction from memory (STM) or throw exception.
fetchTxFromMempoolOrFail :: ExplorerMode m => TxId -> m TxAux
fetchTxFromMempoolOrFail txId =
    view (mpLocalTxs . at txId) <$> getMemPool >>=
    maybeThrow (Internal "Transaction not found in the mempool")

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
    DB.getBlock h >>=
    maybeThrow (Internal "No block found") >>=
    either (const $ throwM $ Internal "Block is genesis block") pure

-- | Get transaction extra from the database, and if you don't find it
-- throw an exception.
getTxExtraOrFail :: MonadDB m => TxId -> m TxExtra
getTxExtraOrFail txId = getTxExtra txId >>= maybeThrow exception
  where
    exception = Internal "Transaction not found"

getTxMain :: ExplorerMode m => TxId -> TxExtra -> m Tx
getTxMain id TxExtra {..} = case teBlockchainPlace of
    Nothing -> view _1 <$> fetchTxFromMempoolOrFail id
    Just (hh, idx) -> do
        mb <- getMainBlock hh
        maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
            atMay (toList $ mb ^. blockTxs) $ fromIntegral idx
