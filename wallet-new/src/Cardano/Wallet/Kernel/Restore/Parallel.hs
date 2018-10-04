{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Restore.Parallel (
      resolveMainBlocks
    , blundsToResolvedBlocks
    , restoreWalletHistoryAsync
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Data.Acid (update)
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Block, Blund, HeaderHash, MainBlock, Undo,
                     mainBlockSlot, undoTx)
import           Pos.Chain.Genesis (GenesisHash, configGenesisHash)
import           Pos.Core (BlockCount (..), getCurrentTimestamp)
import           Pos.Core.Slotting (SlotId (..), flattenSlotId)
import           Pos.DB.Block (getBlock, getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)

import           Data.Conduit as CL
import qualified Data.Conduit.Combinators as CL

import           Cardano.Wallet.API.Types.UnitOfMeasure
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlock (..),
                     RestorationComplete (..))
import           Cardano.Wallet.Kernel.DB.BlockContext (mainBlockContext)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationProgress (..),
                     removeRestoration, walletMeta, walletNode, wallets,
                     wrpCurrentSlot, wrpTargetSlot, wrpThroughput)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeConstraints,
                     NodeStateAdaptor, WithNodeState, defaultGetSlotStart,
                     getCoreConfig, getSecurityParameter, getSlotCount,
                     withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock)
import           Cardano.Wallet.Kernel.Restore.Types
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     WalletId (..), fromRawResolvedBlock, rawResolvedBlock,
                     rawResolvedBlockInputs, rawResolvedContext, rawTimestamp)


blundsToResolvedBlocks :: NodeStateAdaptor IO
                       -> [Blund]
                       -> IO [ResolvedBlock]
blundsToResolvedBlocks node blunds =
    let blocks = CL.runConduit $ do
                    CL.yieldMany blunds
                 .| yieldMainBlock
                 .| CL.sinkList
    in resolveMainBlocks node (runIdentity blocks)

yieldMainBlock :: Monad m => ConduitT Blund (MainBlock, Undo) m ()
yieldMainBlock = do
    b <- CL.await
    case b of
         Nothing           -> pure ()
         Just (Left _, _)  -> pure ()
         Just (Right m, u) -> CL.yield (m, u)

resolveMainBlocks :: NodeStateAdaptor IO
                  -> [(MainBlock, Undo)]
                  -> IO [ResolvedBlock]
resolveMainBlocks node blocks = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    Async.forConcurrently blocks $ \(mb, u) -> do
        withNodeState node $ \_lock -> do
            ctxt  <- mainBlockContext genesisHash mb
            mTime <- defaultGetSlotStart (mb ^. mainBlockSlot)
            now   <- liftIO $ getCurrentTimestamp
            return $ fromRawResolvedBlock UnsafeRawResolvedBlock {
                rawResolvedBlock       = mb
              , rawResolvedBlockInputs = map (map fromJust) $ undoTx u
              , rawTimestamp           = either (const now) identity mTime
              , rawResolvedContext     = ctxt
              }

restoreWalletHistoryAsync :: Kernel.PassiveWallet
                          -> HD.HdRootId
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> IORef WalletRestorationProgress
                          -> Maybe HeaderHash
                            -- ^ The last hash that this wallet has already restored,
                            -- or Nothing to start from the genesis block's successor.
                          -> (HeaderHash, SlotId)
                            -- ^ The block that we are trying to reach via restoration.
                          -> IO ()
restoreWalletHistoryAsync wallet rootId prefilter progress start (tgtHash, tgtSlot) = do
    genesisHash <- configGenesisHash <$> getCoreConfig (wallet ^. walletNode)
    -- 'getFirstGenesisBlockHash' is confusingly named: it returns the hash of
    -- the first block /after/ the genesis block.
    startingPoint <- case start of
        Nothing -> withNode (getFirstGenesisBlockHash genesisHash)
        Just sh ->
            nextHistoricalHash sh >>=
            maybe (throwM $ RestorationSuccessorNotFound sh) pure
    restore genesisHash startingPoint NoTimingData
  where
    wId :: WalletId
    wId = WalletIdHdRnd rootId

    -- Process the restoration of the blocks within the 'EpochIndex' supplied
    -- as input, starting from the given 'HeaderHash'.
    restore :: GenesisHash -> HeaderHash -> TimingData -> IO ()
    restore genesisHash hh timing = do

        -- Update each account's historical checkpoints
        let applyHistorical (mb, prefilteredBlocks, txMetas, rate) = do
                -- Apply the block
                k    <- getSecurityParameter (wallet ^. walletNode)
                ctxt <- withNode $ mainBlockContext genesisHash mb
                mErr <- update (wallet ^. wallets) $
                       ApplyHistoricalBlock k ctxt rootId prefilteredBlocks
                case mErr of
                    Left err -> throwM $ RestorationApplyHistoricalBlockFailed err
                    Right () -> return ()
                -- Update our progress
                slotCount <- getSlotCount (wallet ^. walletNode)
                let flat             = flattenSlotId slotCount
                    blockPerSec      = MeasuredIn . BlockCount . perSecond <$> rate
                    throughputUpdate = maybe identity (set wrpThroughput) blockPerSec
                    slotId           = mb ^. mainBlockSlot
                modifyIORef' progress ( (wrpCurrentSlot .~ flat slotId)
                                      . (wrpTargetSlot  .~ flat tgtSlot)
                                      . throughputUpdate )
                -- Store the TxMetas
                forM_ txMetas (putTxMeta (wallet ^. walletMeta))

        CL.runConduit $ do
               streamMainBlunds genesisHash hh timing
            .| CL.mapM (\(mb,u, rate) ->
                          let b = (Right mb, u)
                          in do (bks, metas) <- prefilter b
                                pure (mb, bks, metas, rate)
                       )
            .| CL.iterM applyHistorical
            .| CL.sinkNull

    -- TODO (@mn): probably should use some kind of bracket to ensure this cleanup happens.
    finish :: IO ()
    finish = do
        k <- getSecurityParameter (wallet ^. walletNode)
        update (wallet ^. wallets) $ RestorationComplete k rootId
        removeRestoration wallet wId

    -- Step forward to the successor of the given block.
    nextHistoricalHash :: HeaderHash -> IO (Maybe HeaderHash)
    nextHistoricalHash hh = withNode $ resolveForwardLink hh

    withNode :: forall a. (NodeConstraints => WithNodeState IO a) -> IO a
    withNode action = withNodeState (wallet ^. walletNode) (\_lock -> action)

    -- Get a block
    getBlockOrThrow :: GenesisHash -> HeaderHash -> IO Block
    getBlockOrThrow genesisHash hh = do
        mBlock <- withNode $ getBlock genesisHash hh
        case mBlock of
           Nothing -> throwM $ RestorationBlockNotFound hh
           Just b  -> return b

    -- Get undo for a mainblock
    -- NOTE: We use this undo information only for input resolution.
    getUndoOrThrow :: GenesisHash -> HeaderHash -> IO Undo
    getUndoOrThrow genesisHash hh = do
        mBlock <- withNode $ getUndo genesisHash hh
        case mBlock of
           Nothing -> throwM $ RestorationUndoNotFound hh
           Just b  -> return b

    -- Get a blund
    getBlundOrThrow :: GenesisHash -> HeaderHash -> IO Blund
    getBlundOrThrow genesisHash hh = do
        b <- Async.async $ getBlockOrThrow genesisHash hh
        u <- Async.async $ getUndoOrThrow genesisHash hh
        Async.waitBoth b u

    streamMainBlunds :: GenesisHash
                     -> HeaderHash
                     -> TimingData
                     -> ConduitT () (MainBlock, Undo, Maybe Rate) IO ()
    streamMainBlunds gh current timer = do
        (rate, timer') <- liftIO (tickTiming 5 timer)
        blund <- liftIO (getBlundOrThrow gh current)

        case blund of
           (Left _, _)  -> pure ()
           (Right m, u) -> CL.yield (m, u, rate)

        if tgtHash == current then
            liftIO finish
          else liftIO (nextHistoricalHash current) >>= \case
            Nothing  -> throwM (RestorationFinishUnreachable tgtHash current)
            Just hh' -> streamMainBlunds gh hh' timer'
