{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

-- API server logic

module Pos.Explorer.Web.Server
       ( explorerServeImpl
       , explorerApp
       , explorerHandlers
       ) where

import           Control.Monad.Catch            (try)
import           Control.Monad.Loops            (unfoldrM)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Maybe                     (fromMaybe, fromJust)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)
import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), withHash)
import qualified Pos.DB                         as DB
import qualified Pos.DB.GState                  as GS
import           Pos.DB.GState.Explorer         as GS (getTxExtra)
import           Pos.Types.Explorer             (TxExtra (..))
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (getLocalTxs)
import           Pos.Types                      (Address (..), HeaderHash,
                                                 MainBlock, Timestamp,
                                                 blockTxs, gbHeader,
                                                 gbhConsensus, mcdSlot, mkCoin,
                                                 prevBlockL, sumCoins,
                                                 unsafeIntegerToCoin, difficultyL,
                                                 unsafeSubCoin)
import           Pos.Txp                        (Tx (..), TxId (..), TxOut (..),
                                                 topsortTxs, txOutAddress,
                                                 _txOutputs)
import           Pos.Util                       (maybeThrow)
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..),
                                                 CAddressSummary (..),
                                                 CBlockEntry (..),
                                                 CBlockSummary (..), CHash,
                                                 CTxEntry (..), CTxId (..),
                                                 CTxSummary (..),
                                                 TxInternal (..), fromCAddress,
                                                 fromCHash', toBlockEntry,
                                                 toBlockSummary, toTxEntry,
                                                 toTxRelative, fromCTxId,
                                                 toPosixTime, toCAddress)
import           Pos.Explorer.Web.Error         (ExplorerError (..))

----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type ExplorerMode m = WorkMode SscGodTossing m

explorerServeImpl :: ExplorerMode m => m Application -> Word16 -> m ()
explorerServeImpl = serveImpl

explorerApp :: ExplorerMode m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode m => SendActions m -> ServerT ExplorerApi m
explorerHandlers sendActions =
    catchExplorerError ... defaultLimit 10 getLastBlocks
    :<|>
    catchExplorerError ... defaultLimit 10 getLastTxs
    :<|>
    catchExplorerError . getBlockSummary
    :<|>
    (\h -> catchExplorerError ... defaultLimit 10 (getBlockTxs h))
    :<|>
    catchExplorerError . getAddressSummary
    :<|>
    catchExplorerError . getTxSummary
  where
    catchExplorerError = try
    f ... g = (f .) . g

defaultLimit
    :: Word                 -- default limit (default offset is always 0)
    -> (Word -> Word -> a)  -- action to transform
    -> Maybe Word
    -> Maybe Word
    -> a
defaultLimit lim action mlim moff =
    action (fromMaybe lim mlim) (fromMaybe 0 moff)

getLastBlocks :: ExplorerMode m => Word -> Word -> m [CBlockEntry]
getLastBlocks lim off = do
    tip <- GS.getTip
    let getNextBlk h _ = fmap (view prevBlockL) $
            DB.getBlockHeader h >>=
            maybeThrow (Internal "Block database is malformed!")
    start <- foldlM getNextBlk tip [0..off]

    let unfolder n h = do
            when (n == 0) $
                fail "limit!"
            MaybeT (DB.getBlock h) >>= \case
                Left gb -> unfolder n (gb ^. prevBlockL)
                Right mb -> (,) <$> lift (toBlockEntry mb) <*>
                            pure (n - 1, mb ^. prevBlockL)
    flip unfoldrM (lim, start) $ \(n, h) -> runMaybeT $ unfolder n h

getLastTxs :: ExplorerMode m => Word -> Word -> m [CTxEntry]
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) = do
    localTxsWithTs <- fmap (take lim . drop off) mempoolTxs

    let lenTxs = length localTxsWithTs
        offLeft = off - lenTxs
        nLeft = offLeft + lim

    blockTxsWithTs <- blockchainTxs offLeft nLeft

    pure $ [toTxEntry (tiTimestamp txi) (tiTx txi) | txi <- localTxsWithTs <> blockTxsWithTs]

getBlockSummary :: ExplorerMode m => CHash -> m CBlockSummary
getBlockSummary (fromCHash' -> h) = do
    mainBlock <- getMainBlock h
    toBlockSummary mainBlock

getBlockTxs :: ExplorerMode m => CHash -> Word -> Word -> m [CTxEntry]
getBlockTxs (fromCHash' -> h) (fromIntegral -> lim) (fromIntegral -> off) = do
    blk <- getMainBlock h
    blkSlotStart <- getBlkSlotStart blk
    let txs = toList $ blk ^. blockTxs
    map (toTxEntry blkSlotStart) . take lim . drop off <$>
        topsortTxsOrFail withHash txs

getAddressSummary :: ExplorerMode m => CAddress -> m CAddressSummary
getAddressSummary cAddr = cAddrToAddr cAddr >>= \addr -> case addr of
    PubKeyAddress sid _ -> do
        balance <- fromMaybe (mkCoin 0) <$> GS.getFtsStake sid
        -- TODO: add number of coins when it's implemented
        -- txs <- allTxs
        let transactions = []
                           -- map (toTxRelative addr) $
                           -- filter (any (\txOut -> _txOutAddress txOut == addr) .
                           --     _txOutputs . tiTx) txs
        return $ CAddressSummary cAddr 0 balance transactions
    _ -> throwM $
         Internal "Non-P2PKH addresses are not supported in Explorer yet"

getTxSummary :: ExplorerMode m => CTxId -> m CTxSummary
getTxSummary cTxId = do
    -- There are two places whence we can fetch a transaction: MemPool and DB.
    txId <- cTxIdToTxId cTxId
    txExtraMaybe <- GS.getTxExtra txId

    when (isNothing txExtraMaybe) $
        throwM $ Internal "Transaction not found"

    let txExtra = fromJust txExtraMaybe
        blockchainPlace = teBlockchainPlace txExtra
        inputOutputs = teInputOutputs txExtra

    let convertTxOutputs outputs = [(toCAddress $ txOutAddress txOut, txOutValue txOut) | txOut <- outputs]

    -- TODO: here and in mempoolTxs/blockchainTxs we do two things wrongly:
    -- 1. If the transaction is found in the MemPool, we return *current
    --    system time* as the time when it was issued.
    -- 2. If the transaction comes from the blockchain, we return *block
    --    slot starting time* as the time when it was issued.
    -- This needs to be fixed.
    (ctsTxTimeIssued, ctsBlockTimeIssued, ctsBlockHeight, ctsOutputs) <-
        case blockchainPlace of
            Nothing -> do
                -- Fetching transaction from MemPool.
                ts <- currentTimeSlotting
                tx <- fetchTxFromMempoolOrFail txId
                let txOutputs = convertTxOutputs $ _txOutputs tx
                pure (Just (toPosixTime ts), Nothing, Nothing, txOutputs)
            Just (headerHash, txIndexInBlock) -> do
                -- Fetching transaction from DB.
                maybeBlock <- DB.getBlock headerHash
                when (isNothing maybeBlock) $
                    throwM $ Internal "TxExtra says tx is in nonexistent block"
                let block = fromJust maybeBlock
                case block of
                    Left gb -> throwM $ Internal "TxExtra says tx is in genesis block"
                    Right mb -> do
                        blkSlotStart <- getBlkSlotStart mb
                        let blockHeight = fromIntegral $ mb ^. difficultyL
                            txMaybe = atMay (toList $ mb ^. blockTxs) (fromIntegral txIndexInBlock)
                        when (isNothing txMaybe) $
                            throwM $ Internal "TxExtra return tx index that is out of bounds"
                        let txOutputs = convertTxOutputs $ _txOutputs $ fromJust txMaybe
                        case blkSlotStart of
                            Nothing -> pure (Nothing, Nothing, Just blockHeight, txOutputs)
                            Just ts -> pure (Just (toPosixTime ts), Just (toPosixTime ts), Just blockHeight, txOutputs)

    let ctsId = cTxId
        ctsRelayedBy = Nothing
        ctsTotalInput = unsafeIntegerToCoin $ sumCoins $ map txOutValue inputOutputs
        ctsInputs = convertTxOutputs inputOutputs
        ctsTotalOutput = unsafeIntegerToCoin $ sumCoins $ map snd ctsOutputs

    when (ctsTotalOutput >= ctsTotalInput) $
        throwM $ Internal "Detected tx with output greater than input"

    let ctsFees = unsafeSubCoin ctsTotalInput ctsTotalOutput
    pure $ CTxSummary {..}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

fetchTxFromMempoolOrFail :: ExplorerMode m => TxId -> m Tx
fetchTxFromMempoolOrFail txId = do
    localTxs <- getLocalTxs
    let filtered = [snd tx | tx <- localTxs, fst tx == txId]

    case filtered of
        []        -> throwM $ Internal "transaction not found in the mempool"
        [tx]      -> pure $ view _1 tx
        otherwise -> throwM $ Internal "multiple transactions with the same id found in the mempool"

mempoolTxs :: ExplorerMode m => m [TxInternal]
mempoolTxs = do
    let mkWhTx (txid, (tx, _, _)) = WithHash tx txid
    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< getLocalTxs
    ts <- currentTimeSlotting

    pure $ map (\tx -> TxInternal (Just ts) (view (_2 . _1) tx)) localTxs

blockchainTxs :: ExplorerMode m => Int -> Int -> m [TxInternal]
blockchainTxs off lim = do
    let unfolder off lim h = do
            when (lim <= 0) $
                fail "Finished"
            MaybeT (DB.getBlock h) >>= \case
                Left gb -> unfolder off lim (gb ^. prevBlockL)
                Right mb -> do
                    let mTxs = mb ^. blockTxs
                    if off >= length mTxs
                        then return ([], (off - length mTxs, lim, mb ^. prevBlockL))
                        else do
                        txs <- topsortTxsOrFail identity $ map withHash $ toList mTxs
                        blkSlotStart <- lift $ getBlkSlotStart mb
                        let neededTxs = take lim $ drop off $ reverse txs
                            blkTxEntries = [TxInternal blkSlotStart (whData tx) | tx <- neededTxs]
                            offLeft = off - length mTxs
                            nLeft = offLeft + lim
                        return (blkTxEntries, (offLeft, nLeft, mb ^. prevBlockL))

    tip <- GS.getTip
    fmap concat $ flip unfoldrM (off, lim, tip) $
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
    either (\_ -> throwM $ Internal "Invalid address!") pure

cTxIdToTxId :: MonadThrow m => CTxId -> m TxId
cTxIdToTxId cTxId =
    fromCTxId cTxId &
    either (\_ -> throwM $ Internal "Invalid transaction id!") pure

getMainBlock :: ExplorerMode m => HeaderHash -> m (MainBlock SscGodTossing)
getMainBlock h =
    DB.getBlock h >>=
    maybeThrow (Internal "No block found") >>=
    either (const $ throwM $ Internal "Block is genesis block") pure
