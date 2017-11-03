{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
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
       , cAddrToAddr
       ) where

import           Universum

import           Control.Lens                         (at)
import qualified Data.ByteString                      as BS
import qualified Data.HashMap.Strict                  as HM
import qualified Data.List.NonEmpty                   as NE
import           Data.Maybe                           (fromMaybe)
import qualified Data.Vector                          as V
import           Formatting                           (build, int, sformat, (%))
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Serokell.Util.Base64                 as B64
import           Servant.API                          ((:<|>) ((:<|>)))
import           Servant.Server                       (Server, ServerT, serve)
import           System.Wlog                          (logDebug)

import           Pos.Communication                    (SendActions)
import           Pos.Crypto                           (WithHash (..), hash, redeemPkBuild,
                                                       withHash)

import qualified Pos.DB.Block                         as DB
import qualified Pos.DB.DB                            as DB

import           Pos.Binary.Class                     (biSize)
import           Pos.Block.Core                       (MainBlock, mainBlockSlot,
                                                       mainBlockTxPayload, mcdSlot)
import           Pos.Block.Types                      (Blund, Undo)
import           Pos.Core                             (AddrType (..), Address (..), Coin,
                                                       EpochIndex, HeaderHash, Timestamp,
                                                       coinToInteger, difficultyL,
                                                       gbHeader, gbhConsensus,
                                                       getChainDifficulty,
                                                       isUnknownAddressType,
                                                       makeRedeemAddress, siEpoch, siSlot,
                                                       sumCoins, timestampToPosix,
                                                       unsafeAddCoin, unsafeIntegerToCoin,
                                                       unsafeSubCoin)
import           Pos.DB.Class                         (MonadDBRead)
import           Pos.Slotting                         (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Configuration                (HasSscConfiguration)
import           Pos.Txp                              (MonadTxpMem, Tx (..), TxAux, TxId,
                                                       TxMap, TxOutAux (..), getLocalTxs,
                                                       getMemPool, mpLocalTxs, taTx,
                                                       topsortTxs, txOutValue, txpTxs,
                                                       _txOutputs)
import           Pos.Util                             (maybeThrow)
import           Pos.Util.Chrono                      (NewestFirst (..))
import           Pos.Web                              (serveImpl)
import           Pos.WorkMode                         (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes       ()
import           Pos.Explorer.Core                    (TxExtra (..))
import           Pos.Explorer.DB                      (getAddrBalance, getAddrHistory,
                                                       getEpochBlocks,
                                                       getLastTransactions, getPageBlocks,
                                                       getTxExtra, getUtxoSum)
import           Pos.Explorer.ExtraContext            (HasGenesisRedeemAddressInfo (..))
import           Pos.Explorer.Web.Api                 (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes         (Byte, CAda (..), CAddress (..),
                                                       CAddressSummary (..),
                                                       CAddressType (..),
                                                       CAddressesFilter (..),
                                                       CBlockEntry (..),
                                                       CBlockSummary (..),
                                                       CGenesisAddressInfo (..),
                                                       CGenesisSummary (..), CHash,
                                                       CTxBrief (..), CTxEntry (..),
                                                       CTxId (..), CTxSummary (..),
                                                       TxInternal (..), convertTxOutputs,
                                                       convertTxOutputsMB, fromCAddress,
                                                       fromCHash, fromCTxId,
                                                       getEpochIndex, getSlotIndex,
                                                       mkCCoin, mkCCoinMB, tiToTxEntry,
                                                       toBlockEntry, toBlockSummary,
                                                       toCAddress, toCHash, toCTxId,
                                                       toTxBrief)
import           Pos.Explorer.Web.Error               (ExplorerError (..))

----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type MainBlund = (MainBlock, Undo)

type ExplorerMode ctx m =
    ( WorkMode ctx m
    , HasGenesisRedeemAddressInfo m
    , HasSscConfiguration
    )

explorerServeImpl
    :: ExplorerMode ctx m
    => m Application
    -> Word16
    -> m ()
explorerServeImpl app port = serveImpl loggingApp "*" port Nothing
  where
    loggingApp = logStdoutDev <$> app

explorerApp :: ExplorerMode ctx m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode ctx m => SendActions m -> ServerT ExplorerApi m
explorerHandlers _sendActions =
      getTotalAda
    :<|>
      getBlocksPage
    :<|>
      getBlocksPagesTotal
    :<|>
      getBlockSummary
    :<|>
      getBlockTxs
    :<|>
      getLastTxs
    :<|>
      getTxSummary
    :<|>
      getAddressSummary
    :<|>
      epochSlotSearch
    :<|>
      getGenesisSummary
    :<|>
      getGenesisPagesTotal
    :<|>
      getGenesisAddressInfo
    :<|>
      getStatsTxs

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

getTotalAda :: ExplorerMode ctx m => m CAda
getTotalAda = do
    utxoSum <- getUtxoSum
    validateUtxoSum utxoSum
    pure $ CAda $ fromInteger utxoSum / 1e6
  where
    validateUtxoSum :: ExplorerMode ctx m => Integer -> m ()
    validateUtxoSum n
        | n < 0 = throwM $ Internal $
            sformat ("Internal tracker of utxo sum has a negative value: "%build) n
        | n > coinToInteger (maxBound :: Coin) = throwM $ Internal $
            sformat ("Internal tracker of utxo sum overflows: "%build) n
        | otherwise = pure ()

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: ExplorerMode ctx m
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- DB.getTipBlock
    pure $ maxBlocks tipBlock
  where
    maxBlocks tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL


-- | Get last blocks with a page parameter. This enables easier paging on the
-- client side and should enable a simple and thin client logic.
-- Currently the pages are in chronological order.
getBlocksPage
    :: ExplorerMode ctx m
    => Maybe Word
    -> Maybe Word
    -> m (Integer, [CBlockEntry])
getBlocksPage mPageNumber mPageSize = do
    let pageSize = toPageSize mPageSize
    -- Get total pages from the blocks.
    totalPages <- getBlocksPagesTotal mPageSize

    -- Initially set on the last page number if page number not defined.
    let pageNumber = fromMaybe totalPages $ toInteger <$> mPageNumber

    -- Make sure the parameters are valid.
    when (pageNumber <= 0) $
        throwM $ Internal "Number of pages must be greater than 0."

    when (pageNumber > totalPages) $
        throwM $ Internal "Number of pages exceeds total pages number."

    -- TODO: Fix in the future.
    when (pageSize /= fromIntegral defaultPageSize) $
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
        :: (DB.MonadBlockDB m, MonadThrow m)
        => Int
        -> m [HeaderHash]
    getPageHHsOrThrow pageNumber = getPageBlocks pageNumber >>=
        maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on page "%build%" found!") pageNumber

-- Either get the block from the @HeaderHash@ or throw an exception.
getBlundOrThrow
    :: (DB.MonadBlockDB m, MonadThrow m)
    => HeaderHash
    -> m Blund
getBlundOrThrow headerHash = DB.blkGetBlund headerHash >>=
    maybeThrow (Internal "Blund with hash cannot be found!")

-- | Get total pages from blocks. Calculated from
-- pageSize we pass to it.
getBlocksPagesTotal
    :: ExplorerMode ctx m
    => Maybe Word
    -> m Integer
getBlocksPagesTotal mPageSize = do
    let pageSize = toPageSize mPageSize
    -- Get total blocks in the blockchain.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Get total pages from the blocks. And we want the page
    -- with the example, the page size 10,
    -- to start with 10 + 1 == 11, not with 10 since with
    -- 10 we'll have an empty page.
    let totalPages = (blocksTotal - 1) `div` pageSize

    -- We start from page 1.
    pure (totalPages + 1)


-- | Get the last page from the blockchain. We use the default 10
-- for the page size since this is called from __explorer only__.
getBlocksLastPage
    :: ExplorerMode ctx m
    => m (Integer, [CBlockEntry])
getBlocksLastPage = getBlocksPage Nothing (Just defaultPageSize)

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
            extra <- getTxExtra txId >>=
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
    -> Maybe Word
    -> Maybe Word
    -> m [CTxBrief]
getBlockTxs cHash mLimit mSkip = do
    let limit = fromIntegral $ fromMaybe defaultPageSize mLimit
    let skip = fromIntegral $ fromMaybe 0 mSkip
    txs <- getMainBlockTxs cHash

    forM (take limit . drop skip $ txs) $ \tx -> do
        extra <- getTxExtra (hash tx) >>=
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

    when (isUnknownAddressType addr) $
        throwM $ Internal "Unknown address type"

    balance <- mkCCoin . fromMaybe minBound <$> getAddrBalance addr
    txIds <- getNewestFirst <$> getAddrHistory addr
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
    getAddressType :: Address -> CAddressType
    getAddressType Address {..} =
        case addrType of
            ATPubKey     -> CPubKeyAddress
            ATScript     -> CScriptAddress
            ATRedeem     -> CRedeemAddress
            ATUnknown {} -> CUnknownAddress

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
        let blockTime           = timestampToPosix <$> blkSlotStart

        -- Get block epoch and slot index
        let blkHeaderSlot       = mb ^. mainBlockSlot
        let epochIndex          = getEpochIndex $ siEpoch blkHeaderSlot
        let slotIndex           = getSlotIndex  $ siSlot  blkHeaderSlot
        let blkHash             = toCHash headerHashBP

        tx <- maybeThrow (Internal "TxExtra return tx index that is out of bounds") $
              atMay (toList $ mb ^. mainBlockTxPayload . txpTxs) (fromIntegral txIndexInBlock)

        let inputOutputsMB      = map (fmap toaOut) $ NE.toList $ teInputOutputs txExtra
        let txOutputs           = convertTxOutputs . NE.toList $ _txOutputs tx

        let totalInputMB        = unsafeIntegerToCoin . sumCoins . map txOutValue <$> sequence inputOutputsMB
        let totalOutput         = unsafeIntegerToCoin $ sumCoins $ map snd txOutputs

        -- Verify that strange things don't happen with transactions
        whenJust totalInputMB $ \totalInput -> when (totalOutput > totalInput) $
            throwM $ Internal "Detected tx with output greater than input"

        pure $ CTxSummary
            { ctsId              = cTxId'
            , ctsTxTimeIssued    = timestampToPosix <$> receivedTime
            , ctsBlockTimeIssued = blockTime
            , ctsBlockHeight     = Just blockHeight
            , ctsBlockEpoch      = Just epochIndex
            , ctsBlockSlot       = Just slotIndex
            , ctsBlockHash       = Just blkHash
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = mkCCoinMB totalInputMB
            , ctsTotalOutput     = mkCCoin totalOutput
            , ctsFees            = mkCCoinMB $ (`unsafeSubCoin` totalOutput) <$> totalInputMB
            , ctsInputs          = map (fmap (second mkCCoin)) $ convertTxOutputsMB inputOutputsMB
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
            , ctsInputs          = map (Just . second mkCCoin) $ convertTxOutputs inputOutputs
            , ctsOutputs         = map (second mkCCoin) txOutputs
            }

data GenesisSummaryInternal = GenesisSummaryInternal
    { gsiNumRedeemed            :: !Int
    , gsiRedeemedAmountTotal    :: !Coin
    , gsiNonRedeemedAmountTotal :: !Coin
    }

getGenesisSummary
    :: ExplorerMode ctx m
    => m CGenesisSummary
getGenesisSummary = do
    grai <- getGenesisRedeemAddressInfo
    redeemAddressInfo <- V.mapM (uncurry getRedeemAddressInfo) grai
    let GenesisSummaryInternal {..} =
            V.foldr folder (GenesisSummaryInternal 0 minBound minBound)
            redeemAddressInfo
    let numTotal = length grai
    pure CGenesisSummary
        { cgsNumTotal = numTotal
        , cgsNumRedeemed = gsiNumRedeemed
        , cgsNumNotRedeemed = numTotal - gsiNumRedeemed
        , cgsRedeemedAmountTotal = mkCCoin gsiRedeemedAmountTotal
        , cgsNonRedeemedAmountTotal = mkCCoin gsiNonRedeemedAmountTotal
        }
  where
    getRedeemAddressInfo
        :: (MonadDBRead m, MonadThrow m)
        => Address -> Coin -> m GenesisSummaryInternal
    getRedeemAddressInfo address initialBalance = do
        currentBalance <- fromMaybe minBound <$> getAddrBalance address
        if currentBalance > initialBalance then
            throwM $ Internal $ sformat
                ("Redeem address "%build%" had "%build%" at genesis, but now has "%build)
                address initialBalance currentBalance
        else
            -- Abusing gsiNumRedeemed here. We'd like to keep
            -- only one wrapper datatype, so we're storing an Int
            -- with a 0/1 value in a field that we call isRedeemed.
            let isRedeemed = if currentBalance == minBound then 1 else 0
                redeemedAmount = initialBalance `unsafeSubCoin` currentBalance
                amountLeft = currentBalance
            in pure $ GenesisSummaryInternal isRedeemed redeemedAmount amountLeft
    folder
        :: GenesisSummaryInternal
        -> GenesisSummaryInternal
        -> GenesisSummaryInternal
    folder
        (GenesisSummaryInternal isRedeemed redeemedAmount amountLeft)
        (GenesisSummaryInternal numRedeemed redeemedAmountTotal nonRedeemedAmountTotal) =
        GenesisSummaryInternal
            { gsiNumRedeemed = numRedeemed + isRedeemed
            , gsiRedeemedAmountTotal = redeemedAmountTotal `unsafeAddCoin` redeemedAmount
            , gsiNonRedeemedAmountTotal = nonRedeemedAmountTotal `unsafeAddCoin` amountLeft
            }

isAddressRedeemed :: MonadDBRead m => Address -> m Bool
isAddressRedeemed address = do
    currentBalance <- fromMaybe minBound <$> getAddrBalance address
    pure $ currentBalance == minBound

getFilteredGrai :: ExplorerMode ctx m => CAddressesFilter -> m (V.Vector (Address, Coin))
getFilteredGrai addrFilt = do
    grai <- getGenesisRedeemAddressInfo
    case addrFilt of
            AllAddresses         ->
                pure grai
            RedeemedAddresses    ->
                V.filterM (isAddressRedeemed . fst) grai
            NonRedeemedAddresses ->
                V.filterM (isAddressNotRedeemed . fst) grai
  where
    isAddressNotRedeemed :: MonadDBRead m => Address -> m Bool
    isAddressNotRedeemed = fmap not . isAddressRedeemed

getGenesisAddressInfo
    :: (ExplorerMode ctx m)
    => Maybe Word  -- ^ pageNumber
    -> Maybe Word  -- ^ pageSize
    -> CAddressesFilter
    -> m [CGenesisAddressInfo]
getGenesisAddressInfo (fmap fromIntegral -> mPage) mPageSize addrFilt = do
    filteredGrai <- getFilteredGrai addrFilt
    let pageNumber    = fromMaybe 1 mPage
        pageSize      = fromIntegral $ toPageSize mPageSize
        skipItems     = (pageNumber - 1) * pageSize
        requestedPage = V.slice skipItems pageSize filteredGrai
    V.toList <$> V.mapM toGenesisAddressInfo requestedPage
  where
    toGenesisAddressInfo :: ExplorerMode ctx m => (Address, Coin) -> m CGenesisAddressInfo
    toGenesisAddressInfo (address, coin) = do
        cgaiIsRedeemed <- isAddressRedeemed address
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
    => Maybe Word
    -> CAddressesFilter
    -> m Integer
getGenesisPagesTotal mPageSize addrFilt = do
    filteredGrai <- getFilteredGrai addrFilt
    pure $ fromIntegral $ (length filteredGrai + pageSize - 1) `div` pageSize
  where
    pageSize = fromIntegral $ toPageSize mPageSize

-- | Search the blocks by epoch and slot. Slot is optional.
epochSlotSearch
    :: ExplorerMode ctx m
    => EpochIndex
    -> Maybe Word16
    -> m [CBlockEntry]
epochSlotSearch epochIndex mSlotIndex = do

    -- [CSE-236] Disable search for epoch only
    -- TODO: Remove restriction if epoch search will be optimized
    when (isNothing mSlotIndex) $
        throwM $ Internal "We currently do not support searching for epochs only."

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    epochBlocksHH   <- getPageHHsOrThrow epochIndex
    blunds          <- forM epochBlocksHH getBlundOrThrow
    cBlocksEntry    <- forM (getEpochSlots mSlotIndex (rights' blunds)) toBlockEntry

    pure cBlocksEntry
  where
    rights' x = [(mb, u) | (Right mb, u) <- x]
    -- Get epoch slot block that's being searched or return all epochs if
    -- the slot is @Nothing@.
    getEpochSlots
        :: Maybe Word16
        -> [MainBlund]
        -> [MainBlund]
    getEpochSlots Nothing           blunds = blunds
    getEpochSlots (Just slotIndex) blunds = filter filterBlundsBySlotIndex blunds
      where
        getBlundSlotIndex
            :: MainBlund
            -> Word16
        getBlundSlotIndex blund = getSlotIndex $ siSlot $ fst blund ^. mainBlockSlot

        filterBlundsBySlotIndex
            :: MainBlund
            -> Bool
        filterBlundsBySlotIndex blund = getBlundSlotIndex blund == slotIndex

    -- Either get the @HeaderHash@es from the @Epoch@ or throw an exception.
    getPageHHsOrThrow
        :: (DB.MonadBlockDB m, MonadThrow m)
        => EpochIndex
        -> m [HeaderHash]
    getPageHHsOrThrow epoch = getEpochBlocks epoch >>=
        maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on epoch "%build%" found!") epoch


getStatsTxs
    :: forall ctx m. ExplorerMode ctx m
    => Maybe Word
    -> m (Integer, [(CTxId, Byte)])
getStatsTxs mPageNumber = do
    -- Get blocks from the requested page
    blocksPage <- getBlocksPage mPageNumber (Just defaultPageSize)

    blockPageTxsInfo <- getBlockPageTxsInfo blocksPage
    pure blockPageTxsInfo
  where
    getBlockPageTxsInfo
        :: (Integer, [CBlockEntry])
        -> m (Integer, [(CTxId, Byte)])
    getBlockPageTxsInfo (blockPageNumber, cBlockEntries) = do
        blockTxsInfo <- blockPageTxsInfo
        pure (blockPageNumber, blockTxsInfo)
      where
        cHashes :: [CHash]
        cHashes = cbeBlkHash <$> cBlockEntries

        blockPageTxsInfo :: m [(CTxId, Byte)]
        blockPageTxsInfo = concatForM cHashes getBlockTxsInfo

        getBlockTxsInfo
            :: CHash
            -> m [(CTxId, Byte)]
        getBlockTxsInfo cHash = do
            txs <- getMainBlockTxs cHash
            pure $ txToTxIdSize <$> txs
          where
            txToTxIdSize :: Tx -> (CTxId, Byte)
            txToTxIdSize tx = (toCTxId $ hash tx, biSize tx)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

defaultPageSize :: Word
defaultPageSize = 10

toPageSize :: Maybe Word -> Integer
toPageSize = fromIntegral . fromMaybe defaultPageSize

getMainBlockTxs :: ExplorerMode ctx m => CHash -> m [Tx]
getMainBlockTxs cHash = do
    hash' <- unwrapOrThrow $ fromCHash cHash
    blk   <- getMainBlock hash'
    txs   <- topsortTxsOrFail withHash $ toList $ blk ^. mainBlockTxPayload . txpTxs

    pure txs

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

getBlkSlotStart :: MonadSlots ctx m => MainBlock -> m (Maybe Timestamp)
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
            -- the decoded address can be both the RSCoin address and the Cardano address.
            -- * RSCoin address == 32 bytes
            -- * Cardano address >= 34 bytes
            if (BS.length addr == 32)
                then pure $ makeRedeemAddress $ redeemPkBuild addr
                else either badCardanoAddress pure (fromCAddress cAddr)
        Nothing ->
            -- cAddr is in Cardano address format or it's not valid
            either badCardanoAddress pure (fromCAddress cAddr)
  where

    badCardanoAddress = const $ throwM $ Internal "Invalid Cardano address!"

-- | Deserialize transaction ID.
-- Throw exception on failure.
cTxIdToTxId :: MonadThrow m => CTxId -> m TxId
cTxIdToTxId cTxId = either exception pure (fromCTxId cTxId)
  where
    exception = const $ throwM $ Internal "Invalid transaction id!"

getMainBlund :: ExplorerMode ctx m => HeaderHash -> m MainBlund
getMainBlund h = do
    (blk, undo) <- DB.blkGetBlund h >>= maybeThrow (Internal "No block found")
    either (const $ throwM $ Internal "Block is genesis block") (pure . (,undo)) blk

getMainBlock :: ExplorerMode ctx m => HeaderHash -> m MainBlock
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
