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
import           Data.Maybe                     (fromMaybe)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Server, ServerT, serve)
import Data.Time.Clock.POSIX (getPOSIXTime)
import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Crypto                     (WithHash (..), withHash)
import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.GState                  as GS
import           Pos.Slotting                   (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), getLocalTxs, topsortTxs,
                                                 txOutAddress)
import           Pos.Types                      (Address (..), HeaderHash, MainBlock, SlotId, Timestamp (..),
                                                 Timestamp, blockTxs, gbHeader,
                                                 gbhConsensus, mcdSlot, mkCoin,
                                                 prevBlockL)
import           Pos.Util                       (maybeThrow)
import Pos.Util.TimeWarp (mcs)
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CBlockEntry (..), CBlockSummary (..),
                                                 CHash, CTxEntry (..), TxInternal (..),
                                                 fromCAddress, fromCHash', toBlockEntry,
                                                 toBlockSummary, toTxDetailed, toTxEntry)
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
      apiAddressSummary
  where
    apiBlocksLast     = catchExplorerError ... defaultLimit 10 getLastBlocks
    apiBlocksSummary  = catchExplorerError . getBlockSummary
    apiBlocksTxs      = (\h -> catchExplorerError ... defaultLimit 10 (getBlockTxs h))
    apiTxsLast        = catchExplorerError ... defaultLimit 10 getLastTxs
    apiAddressSummary = catchExplorerError . getAddressSummary

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
getLastTxs (fromIntegral -> lim) (fromIntegral -> off) =
    take lim . drop off .
        map (\txi -> toTxEntry (tiTimestamp txi) (tiTx txi)) <$> allTxs

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
        txs <- allTxs
        let transactions = map (toTxDetailed addr) $
                           filter (any (\txOut -> txOutAddress txOut == addr) .
                               _txOutputs . tiTx) txs
        return $ CAddressSummary cAddr 0 balance transactions
    _ -> throwM $
         Internal "Non-P2PKH addresses are not supported in Explorer yet"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getCurrentTime
    :: (MonadIO m, MonadThrow m)
    => m Timestamp
getCurrentTime =
    Timestamp . mcs . round . (* 1e6) <$>
    liftIO getPOSIXTime

getSlotStartOrFail
    :: (MonadSlots m, MonadThrow m)
    => SlotId -> m Timestamp
getSlotStartOrFail sid =
    getSlotStart sid >>=
    maybeThrow (Internal "Slotting isn't initialized")

allTxs :: ExplorerMode m => m [TxInternal]
allTxs = do
    let mkWhTx (txid, (tx, _, _)) = WithHash tx txid
    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< getLocalTxs
    ts <- getCurrentTime

    let localTxEntries = map (\tx -> TxInternal ts (view (_2 . _1) tx)) localTxs

    tip <- GS.getTip
    let unfolder h = do
            MaybeT (DB.getBlock @SscGodTossing h) >>= \case
                Left gb -> unfolder (gb ^. prevBlockL)
                Right mb -> do
                    let mTxs = mb ^. blockTxs
                    txs <- topsortTxsOrFail identity $ map withHash $ toList mTxs
                    blkSlotStart <- lift $ getSlotStartOrFail $
                                    mb ^. gbHeader . gbhConsensus . mcdSlot
                    let blkTxEntries = map (\tx -> TxInternal blkSlotStart (whData tx)) $
                                       reverse txs
                    return (blkTxEntries, mb ^. prevBlockL)

    blockTxEntries <- fmap concat $ flip unfoldrM tip $
        \h -> runMaybeT $ unfolder h

    return $ localTxEntries <> blockTxEntries

getBlkSlotStart
    :: (MonadSlots m, MonadThrow m)
    => MainBlock ssc -> m Timestamp
getBlkSlotStart blk = getSlotStartOrFail $ blk ^. gbHeader . gbhConsensus . mcdSlot

topsortTxsOrFail :: MonadThrow m => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr =
    fromCAddress cAddr &
    either (\_ -> throwM $ Internal "Invalid address!") pure

getMainBlock :: ExplorerMode m => HeaderHash -> m (MainBlock SscGodTossing)
getMainBlock h =
    DB.getBlock h >>=
    maybeThrow (Internal "No block found") >>=
    either (const $ throwM $ Internal "Block is genesis block") pure
