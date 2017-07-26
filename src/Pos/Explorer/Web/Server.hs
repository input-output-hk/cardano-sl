{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
       , getBlocksLastPage
       ) where

import           Universum

import           Control.Lens                   (at)
import           Control.Monad.Catch            (try)
import qualified Data.ByteString                as BS
import qualified Data.HashMap.Strict            as HM
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe)
import           Formatting                     (build, int, sformat, (%))
import           Network.Wai                    (Application)
import qualified Serokell.Util.Base64           as B64
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)
import           System.Wlog                    (logDebug)

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), hash, redeemPkBuild,
                                                 withHash)

import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.DB                      as DB

import           Pos.Block.Core                 (MainBlock, mainBlockSlot,
                                                 mainBlockTxPayload, mcdSlot)
import           Pos.Block.Types                (Blund, Undo)
import           Pos.Context                    (genesisUtxoM)
import           Pos.DB.Class                   (MonadDBRead)
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (MonadTxpMem, Tx (..), TxAux, TxId, TxMap,
                                                 TxOutAux (..), getLocalTxs, getMemPool,
                                                 mpLocalTxs, taTx, topsortTxs, txOutValue,
                                                 txpTxs, utxoToAddressCoinPairs,
                                                 _txOutputs)
import           Pos.Types                      (Address (..), Coin, EpochIndex,
                                                 HeaderHash, Timestamp, difficultyL,
                                                 gbHeader, gbhConsensus,
                                                 getChainDifficulty, makeRedeemAddress,
                                                 mkCoin, siEpoch, siSlot, sumCoins,
                                                 unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Util                       (maybeThrow)
import           Pos.Util.Chrono                (NewestFirst (..))
import           Pos.Web                        (serveImplNoTLS)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer                   (TxExtra (..), getEpochBlocks,
                                                 getLastTransactions, getPageBlocks,
                                                 getTxExtra)
import qualified Pos.Explorer                   as EX (getAddrBalance, getAddrHistory,
                                                       getTxExtra)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CAddressType (..), CBlockEntry (..),
                                                 CBlockSummary (..),
                                                 CGenesisAddressInfo (..),
                                                 CGenesisSummary (..), CHash,
                                                 CTxBrief (..), CTxEntry (..), CTxId (..),
                                                 CTxSummary (..), TxInternal (..),
                                                 convertTxOutputs, fromCAddress,
                                                 fromCHash, fromCTxId, getEpochIndex,
                                                 getSlotIndex, mkCCoin, tiToTxEntry,
                                                 toBlockEntry, toBlockSummary, toCAddress,
                                                 toCHash, toPosixTime, toTxBrief)
import           Pos.Explorer.Web.Error         (ExplorerError (..))


----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------
type MainBlund ssc = (MainBlock ssc, Undo)

type ExplorerMode ctx m = WorkMode SscGodTossing ctx m

explorerServeImpl
    :: ExplorerMode ctx m
    => m Application
    -> Word16
    -> m ()
explorerServeImpl = flip serveImplNoTLS "*"

explorerApp :: ExplorerMode ctx m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode ctx m => SendActions m -> ServerT ExplorerApi m
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
    :<|>
      apiGenesisSummary
    :<|>
      apiGenesisPagesTotal
    :<|>
      apiGenesisAddressInfo
  where
    apiBlocksPages        = getBlocksPagesDefault
    apiBlocksPagesTotal   = getBlocksPagesTotalDefault
    apiBlocksSummary      = catchExplorerError . getBlockSummary
    apiBlocksTxs          = getBlockTxsDefault
    apiTxsLast            = catchExplorerError getLastTxs
    apiTxsSummary         = catchExplorerError . getTxSummary
    apiAddressSummary     = catchExplorerError . getAddressSummary
    apiEpochSlotSearch    = tryEpochSlotSearch
    apiGenesisSummary     = catchExplorerError getGenesisSummary
    apiGenesisPagesTotal  = getGenesisPagesTotalDefault
    apiGenesisAddressInfo = getGenesisAddressInfoDefault

    catchExplorerError    = try

    getBlocksPagesDefault page size  =
        catchExplorerError $ getBlocksPage page (defaultPageSize size)

    getBlocksPagesTotalDefault size  =
        catchExplorerError $ getBlocksPagesTotal (defaultPageSize size)

    getBlockTxsDefault hash' limit skip =
        catchExplorerError $ getBlockTxs hash' (defaultLimit limit) (defaultSkip skip)

    tryEpochSlotSearch epoch maybeSlot =
        catchExplorerError $ epochSlotSearch epoch maybeSlot

    getGenesisPagesTotalDefault size =
        catchExplorerError $ getGenesisPagesTotal (defaultPageSize size)

    getGenesisAddressInfoDefault page size =
        catchExplorerError $ getGenesisAddressInfo page (defaultPageSize size)

    defaultPageSize size = (fromIntegral $ fromMaybe 10 size)
    defaultLimit limit   = (fromIntegral $ fromMaybe 10 limit)
    defaultSkip  skip    = (fromIntegral $ fromMaybe 0  skip)

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: ExplorerMode ctx m
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- DB.getTipBlock @SscGodTossing

    pure $ maxBlocks tipBlock
  where
    maxBlocks tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL


-- | Get last blocks with a page parameter. This enables easier paging on the
-- client side and should enable a simple and thin client logic.
-- Currently the pages are in chronological order.
getBlocksPage
    :: ExplorerMode ctx m
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
    blunds          <- forM pageBlocksHH getBlundOrThrow
    cBlocksEntry    <- forM (rights' blunds) toBlockEntry

    -- Return total pages and the blocks. We start from page 1.
    pure (totalPages, reverse cBlocksEntry)
  where
    rights' x = [(mb, u) | (Right mb, u) <- x]

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
getBlundOrThrow
    :: (DB.MonadBlockDB SscGodTossing m, MonadThrow m)
    => HeaderHash
    -> m (Blund SscGodTossing)
getBlundOrThrow headerHash = DB.blkGetBlund headerHash >>=
    maybeThrow (Internal "Blund with hash cannot be found!")

-- | Get total pages from blocks. Calculated from
-- pageSize we pass to it.
getBlocksPagesTotal
    :: ExplorerMode ctx m
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
    :: ExplorerMode ctx m
    => m (Integer, [CBlockEntry])
getBlocksLastPage = getBlocksPage Nothing pageSize
  where
    pageSize :: Word
    pageSize = 10


-- | Get last transactions from the blockchain.
getLastTxs
    :: ExplorerMode ctx m
    => m [CTxEntry]
getLastTxs = do
    mempoolTxs     <- getMempoolTxs
    blockTxsWithTs <- getBlockchainLastTxs

    -- We take the mempool txs first, then topsorted blockchain ones.
    let newTxs      = mempoolTxs <> blockTxsWithTs

    pure $ tiToTxEntry <$> newTxs
  where
    -- Get last transactions from the blockchain.
    getBlockchainLastTxs
        :: ExplorerMode ctx m
        => m [TxInternal]
    getBlockchainLastTxs = do
        mLastTxs     <- getLastTransactions
        let lastTxs   = fromMaybe [] mLastTxs
        let lastTxsWH = map withHash lastTxs

        forM lastTxsWH toTxInternal
      where
        -- Convert transaction to TxInternal.
        toTxInternal
            :: (MonadThrow m, MonadDBRead m)
            => WithHash Tx
            -> m TxInternal
        toTxInternal (WithHash tx txId) = do
            extra <- EX.getTxExtra txId >>=
                maybeThrow (Internal "No extra info for tx in DB!")
            pure $ TxInternal extra tx


-- | Get block summary.
getBlockSummary
    :: ExplorerMode ctx m
    => CHash
    -> m CBlockSummary
getBlockSummary cHash = do
    h <- unwrapOrThrow $ fromCHash cHash
    mainBlund <- getMainBlund h
    toBlockSummary mainBlund


-- | Get transactions from a block.
getBlockTxs
    :: ExplorerMode ctx m
    => CHash
    -> Word
    -> Word
    -> m [CTxBrief]
getBlockTxs cHash (fromIntegral -> lim) (fromIntegral -> off) = do
    h   <- unwrapOrThrow $ fromCHash cHash
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
    :: ExplorerMode ctx m
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


-- | Get transaction summary from transaction id. Looks at both the database
-- and the memory (mempool) for the transaction. What we have at the mempool
-- are transactions that have to be written in the blockchain.
getTxSummary
    :: ExplorerMode ctx m
    => CTxId
    -> m CTxSummary
getTxSummary cTxId = do
    -- There are two places whence we can fetch a transaction: MemPool and DB.
    -- However, TxExtra should be added in the DB when a transaction is added
    -- to MemPool. So we start with TxExtra and then figure out whence to fetch
    -- the rest.
    txId                   <- cTxIdToTxId cTxId
    -- Get from database, @TxExtra
    txExtra                <- getTxExtra txId

    -- If we found @TxExtra@ that means we found something saved on the
    -- blockchain and we don't have to fetch @MemPool@. But if we don't find
    -- anything on the blockchain, we go searching in the @MemPool@.
    if isJust txExtra
      then getTxSummaryFromBlockchain cTxId
      else getTxSummaryFromMemPool cTxId

  where
    -- Get transaction from blockchain (the database).
    getTxSummaryFromBlockchain
        :: (ExplorerMode ctx m)
        => CTxId
        -> m CTxSummary
    getTxSummaryFromBlockchain cTxId' = do
        txId                   <- cTxIdToTxId cTxId'
        txExtra                <- getTxExtraOrFail txId

        -- Return transaction extra (txExtra) fields
        let mBlockchainPlace    = teBlockchainPlace txExtra
        blockchainPlace        <- maybeThrow (Internal "No blockchain place.") mBlockchainPlace

        let headerHashBP        = fst blockchainPlace
        let txIndexInBlock      = snd blockchainPlace

        mb                     <- getMainBlock headerHashBP
        blkSlotStart           <- getBlkSlotStart mb

        let blockHeight         = fromIntegral $ mb ^. difficultyL
        let receivedTime        = teReceivedTime txExtra
        let blockTime           = toPosixTime <$> blkSlotStart

        -- Get block epoch and slot index
        let blkHeaderSlot       = mb ^. mainBlockSlot
        let epochIndex          = getEpochIndex $ siEpoch blkHeaderSlot
        let slotIndex           = getSlotIndex  $ siSlot  blkHeaderSlot
        let blkHash             = toCHash headerHashBP

        tx <- maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
              atMay (toList $ mb ^. mainBlockTxPayload . txpTxs) (fromIntegral txIndexInBlock)

        let inputOutputs        = map toaOut $ NE.toList $ teInputOutputs txExtra
        let txOutputs           = convertTxOutputs . NE.toList $ _txOutputs tx

        let totalInput          = unsafeIntegerToCoin $ sumCoins $ map txOutValue inputOutputs
        let totalOutput         = unsafeIntegerToCoin $ sumCoins $ map snd txOutputs

        -- Verify that strange things don't happen with transactions
        when (totalOutput > totalInput) $
            throwM $ Internal "Detected tx with output greater than input"

        pure $ CTxSummary
            { ctsId              = cTxId'
            , ctsTxTimeIssued    = Just $ toPosixTime receivedTime
            , ctsBlockTimeIssued = blockTime
            , ctsBlockHeight     = Just blockHeight
            , ctsBlockEpoch      = Just epochIndex
            , ctsBlockSlot       = Just slotIndex
            , ctsBlockHash       = Just blkHash
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = mkCCoin totalInput
            , ctsTotalOutput     = mkCCoin totalOutput
            , ctsFees            = mkCCoin $ unsafeSubCoin totalInput totalOutput
            , ctsInputs          = map (second mkCCoin) $ convertTxOutputs inputOutputs
            , ctsOutputs         = map (second mkCCoin) txOutputs
            }

    -- Get transaction from mempool (the memory).
    getTxSummaryFromMemPool
        :: (ExplorerMode ctx m)
        => CTxId
        -> m CTxSummary
    getTxSummaryFromMemPool cTxId' = do
        txId                   <- cTxIdToTxId cTxId'
        tx                     <- fetchTxFromMempoolOrFail txId

        let inputOutputs        = NE.toList . _txOutputs $ taTx tx
        let txOutputs           = convertTxOutputs inputOutputs

        let totalInput          = unsafeIntegerToCoin $ sumCoins $ map txOutValue inputOutputs
        let totalOutput         = unsafeIntegerToCoin $ sumCoins $ map snd txOutputs

        -- Verify that strange things don't happen with transactions
        when (totalOutput > totalInput) $
            throwM $ Internal "Detected tx with output greater than input"

        pure $ CTxSummary
            { ctsId              = cTxId'
            , ctsTxTimeIssued    = Nothing
            , ctsBlockTimeIssued = Nothing
            , ctsBlockHeight     = Nothing
            , ctsBlockEpoch      = Nothing
            , ctsBlockSlot       = Nothing
            , ctsBlockHash       = Nothing
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = mkCCoin totalInput
            , ctsTotalOutput     = mkCCoin totalOutput
            , ctsFees            = mkCCoin $ unsafeSubCoin totalInput totalOutput
            , ctsInputs          = map (second mkCCoin) $ convertTxOutputs inputOutputs
            , ctsOutputs         = map (second mkCCoin) txOutputs
            }

getRedeemAddressCoinPairs :: ExplorerMode ctx m => m [(Address, Coin)]
getRedeemAddressCoinPairs = do
    genesisUtxo <- genesisUtxoM
    let addressCoinPairs = utxoToAddressCoinPairs genesisUtxo
        redeemOnly = filter (isRedeemAddress . fst) addressCoinPairs
    pure redeemOnly

isRedeemAddress :: Address -> Bool
isRedeemAddress (RedeemAddress _) = True
isRedeemAddress _                 = False

isAddressRedeemed :: MonadDBRead m => Address -> Coin -> m Bool
isAddressRedeemed address initialBalance = do
  currentBalance <- fromMaybe (mkCoin 0) <$> EX.getAddrBalance address
  pure $ currentBalance /= initialBalance

getGenesisSummary
    :: ExplorerMode ctx m
    => m CGenesisSummary
getGenesisSummary = do
    redeemAddressCoinPairs <- getRedeemAddressCoinPairs
    cgsNumRedeemed <- length <$> filterM (uncurry isAddressRedeemed) redeemAddressCoinPairs
    pure CGenesisSummary {cgsNumTotal = length redeemAddressCoinPairs, ..}

getGenesisAddressInfo
    :: (ExplorerMode ctx m)
    => Maybe Word  -- ^ pageNumber
    -> Word        -- ^ pageSize
    -> m [CGenesisAddressInfo]
getGenesisAddressInfo (fmap fromIntegral -> mPage) (fromIntegral -> pageSize) = do
    redeemAddressCoinPairs <- getRedeemAddressCoinPairs
    let pageNumber    = fromMaybe 1 mPage
        skipItems     = (pageNumber - 1) * pageSize
        requestedPage = take pageSize $ drop skipItems redeemAddressCoinPairs
    mapM toGenesisAddressInfo requestedPage
  where
    toGenesisAddressInfo :: ExplorerMode ctx m => (Address, Coin) -> m CGenesisAddressInfo
    toGenesisAddressInfo (address, coin) = do
        cgaiIsRedeemed <- isAddressRedeemed address coin
        -- Commenting out RSCoin address until it can actually be displayed.
        -- See comment in src/Pos/Explorer/Web/ClientTypes.hs for more information.
        pure CGenesisAddressInfo
            { cgaiCardanoAddress = toCAddress address
            -- , cgaiRSCoinAddress  = toCAddress address
            , cgaiGenesisAmount  = mkCCoin coin
            , ..
            }

getGenesisPagesTotal
    :: ExplorerMode ctx m
    => Word
    -> m Integer
getGenesisPagesTotal (fromIntegral -> pageSize) = do
    redeemAddressCoinPairs <- getRedeemAddressCoinPairs
    pure $ fromIntegral $ (length redeemAddressCoinPairs + pageSize - 1) `div` pageSize

-- | Search the blocks by epoch and slot. Slot is optional.
epochSlotSearch
    :: ExplorerMode ctx m
    => EpochIndex
    -> Maybe Word16
    -> m [CBlockEntry]
epochSlotSearch epochIndex slotIndex = do

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    epochBlocksHH   <- getPageHHsOrThrow epochIndex
    blunds          <- forM epochBlocksHH getBlundOrThrow
    cBlocksEntry    <- forM (getEpochSlots slotIndex (rights' blunds)) toBlockEntry

    pure cBlocksEntry
  where
    rights' x = [(mb, u) | (Right mb, u) <- x]
    -- Get epoch slot block that's being searched or return all epochs if
    -- the slot is @Nothing@.
    getEpochSlots
        :: Maybe Word16
        -> [MainBlund SscGodTossing]
        -> [MainBlund SscGodTossing]
    getEpochSlots Nothing           blunds = blunds
    getEpochSlots (Just slotIndex') blunds = filter filterBlundsBySlotIndex blunds
      where
        getBlundSlotIndex
            :: MainBlund SscGodTossing
            -> Word16
        getBlundSlotIndex blund = getSlotIndex $ siSlot $ fst blund ^. mainBlockSlot

        filterBlundsBySlotIndex
            :: MainBlund SscGodTossing
            -> Bool
        filterBlundsBySlotIndex blund = getBlundSlotIndex blund == slotIndex'

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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


makeTxBrief :: Tx -> TxExtra -> CTxBrief
makeTxBrief tx extra = toTxBrief (TxInternal extra tx)

unwrapOrThrow :: ExplorerMode ctx m => Either Text a -> m a
unwrapOrThrow = either (throwM . Internal) pure

-- | Get transaction from memory (STM) or throw exception.
fetchTxFromMempoolOrFail :: ExplorerMode ctx m => TxId -> m TxAux
fetchTxFromMempoolOrFail txId = do
    memPoolTxs        <- localMemPoolTxs
    let memPoolTxsSize = HM.size memPoolTxs

    logDebug $ sformat ("Mempool size "%int%" found!") memPoolTxsSize

    let maybeTxAux = memPoolTxs ^. at txId
    maybeThrow (Internal "Transaction missing in MemPool!") maybeTxAux

  where
    -- type TxMap = HashMap TxId TxAux
    localMemPoolTxs
        :: (MonadIO m, MonadTxpMem ext ctx m)
        => m TxMap
    localMemPoolTxs = do
      memPool <- getMemPool
      pure $ memPool ^. mpLocalTxs

getMempoolTxs :: ExplorerMode ctx m => m [TxInternal]
getMempoolTxs = do

    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< tlocalTxs

    fmap catMaybes . forM localTxs $ \(id, txAux) -> do
        mextra <- getTxExtra id
        forM mextra $ \extra -> pure $ TxInternal extra (taTx txAux)
  where
    tlocalTxs :: (MonadIO m, MonadTxpMem ext ctx m) => m [(TxId, TxAux)]
    tlocalTxs = getLocalTxs

    mkWhTx :: (TxId, TxAux) -> WithHash Tx
    mkWhTx (txid, txAux) = WithHash (taTx txAux) txid

getBlkSlotStart :: MonadSlots m => MainBlock ssc -> m (Maybe Timestamp)
getBlkSlotStart blk = getSlotStart $ blk ^. gbHeader . gbhConsensus . mcdSlot

topsortTxsOrFail :: (MonadThrow m, Eq a) => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

-- | Deserialize Cardano or RSCoin address and convert it to Cardano address.
-- Throw exception on failure.
cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr@(CAddress rawAddrText) =
    -- Try decoding address as base64. If both decoders succeed,
    -- the output of the first one is returned
    let mDecodedBase64 =
            rightToMaybe (B64.decode rawAddrText) <|>
            rightToMaybe (B64.decodeUrl rawAddrText)
    in case mDecodedBase64 of
        Just addr -> do
            -- cAddr is in RSCoin address format, converting to equivalent Cardano address
            -- Originally taken from:
            -- * cardano-sl/tools/src/keygen/Avvm.hs
            -- * cardano-sl/tools/src/addr-convert/Main.hs
            unless (BS.length addr == 32) $
                throwM badAddressLength
            pure $ makeRedeemAddress $ redeemPkBuild addr
        Nothing ->
            -- cAddr is in Cardano address format
            either badCardanoAddress pure (fromCAddress cAddr)
  where
    badAddressLength = Internal "Address length is not equal to 32, can't be redeeming pk"
    badCardanoAddress = const $ throwM $ Internal "Invalid Cardano address!"

-- | Deserialize transaction ID.
-- Throw exception on failure.
cTxIdToTxId :: MonadThrow m => CTxId -> m TxId
cTxIdToTxId cTxId = either exception pure (fromCTxId cTxId)
  where
    exception = const $ throwM $ Internal "Invalid transaction id!"

getMainBlund :: ExplorerMode ctx m => HeaderHash -> m (MainBlund SscGodTossing)
getMainBlund h = do
    (blk, undo) <- DB.blkGetBlund h >>= maybeThrow (Internal "No block found")
    either (const $ throwM $ Internal "Block is genesis block") (pure . (,undo)) blk

getMainBlock :: ExplorerMode ctx m => HeaderHash -> m (MainBlock SscGodTossing)
getMainBlock = fmap fst . getMainBlund

-- | Get transaction extra from the database, and if you don't find it
-- throw an exception.
getTxExtraOrFail :: MonadDBRead m => TxId -> m TxExtra
getTxExtraOrFail txId = getTxExtra txId >>= maybeThrow exception
  where
    exception = Internal "Transaction not found"

getTxMain :: ExplorerMode ctx m => TxId -> TxExtra -> m Tx
getTxMain id TxExtra {..} = case teBlockchainPlace of
    Nothing -> taTx <$> fetchTxFromMempoolOrFail id
    Just (hh, idx) -> do
        mb <- getMainBlock hh
        maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
            atMay (toList $ mb ^. mainBlockTxPayload . txpTxs) $ fromIntegral idx
