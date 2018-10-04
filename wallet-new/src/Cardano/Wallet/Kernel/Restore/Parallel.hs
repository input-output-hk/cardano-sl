{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}
module Cardano.Wallet.Kernel.Restore.Parallel (
      resolveMainBlocks
    , allBlundsForEpoch
    , blundsToResolvedBlocks
    , restoreWalletHistoryAsync
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Control.Lens (to, _Right, _head)
import           Data.Acid (update)
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Block, Blund, HeaderHash, MainBlock, Undo,
                     headerHash, mainBlockSlot, undoTx)
import           Pos.Chain.Genesis (GenesisHash, configGenesisHash)
import           Pos.Core (BlockCount (..), getCurrentTimestamp)
import           Pos.Core.Chrono (NewestFirst, toOldestFirst)
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     SlotId (..), flattenSlotId, getEpochOrSlot)
import           Pos.DB.Block (getFirstGenesisBlockHash, loadBlundsWhile,
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
                 .| CL.mapWhile mainBlock
                 .| CL.sinkList
    in resolveMainBlocks node (runIdentity blocks)

-- Returns 'Just' if the given 'Blund' is relative to a 'MainBlock', 'Nothing'
-- otherwise.
mainBlock :: Blund -> Maybe (MainBlock, Undo)
mainBlock (Left _, _)   = Nothing
mainBlock (Right mb, u) = Just (mb, u)


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

-- | When given as input an 'EpochIndex' and the 'HeaderHash' of where this
-- 'EpochIndex' starts, returns all the 'Blund's between 'EpochIndex' and
-- 'EpochIndex + 1'.
allBlundsForEpoch :: NodeStateAdaptor IO
                  -> EpochIndex
                  -> HeaderHash
                  -> IO (NewestFirst [] Blund)
allBlundsForEpoch node targetEpoch epochStart = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    withNodeState node $ \_lock -> do
        loadBlundsWhile genesisHash thisEpoch epochStart
  where
      thisEpoch :: Block -> Bool
      thisEpoch b =
          case getEpochOrSlot b of
               (EpochOrSlot (Left epochIndex)) ->
                   epochIndex == targetEpoch
               (EpochOrSlot (Right slotId))    ->
                   (siEpoch slotId) == targetEpoch


restoreWalletHistoryAsync :: Kernel.PassiveWallet
                          -> HD.HdRootId
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> IORef WalletRestorationProgress
                          -> Maybe (HeaderHash, SlotId)
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
        Nothing -> (genesisEpoch,) <$> withNode (getFirstGenesisBlockHash genesisHash)
        -- TODO(adn): Can 'nextHistoricalHash' bring us into the next Epoch?
        Just (sh, sid) ->
            nextHistoricalHash sh >>=
            maybe (throwM $ RestorationSuccessorNotFound sh)
                  (pure . (siEpoch sid,))
    restore genesisHash startingPoint NoTimingData
  where
    wId :: WalletId
    wId = WalletIdHdRnd rootId

    genesisEpoch :: EpochIndex
    genesisEpoch = EpochIndex 0

    -- Process the restoration of the blocks within the 'EpochIndex' supplied
    -- as input, starting from the given 'HeaderHash'.
    restore :: GenesisHash -> (EpochIndex, HeaderHash) -> TimingData -> IO ()
    restore genesisHash (!epoch, startingPointWithinEpoch) timing = do

        -- Updating the average rate every 5 blocks.
        (rate, timing') <- tickTiming 5 timing

        -- Update each account's historical checkpoints
        blunds <- allBlundsForEpoch (wallet ^. walletNode) epoch startingPointWithinEpoch

        let applyHistorical (mb, prefilteredBlocks, txMetas) = do
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
               CL.yieldMany (toList . toOldestFirst $ blunds)
            .| CL.mapWhile mainBlock
            .| CL.mapM (\(mb,u) -> let b = (Right mb, u)
                                       in do (bks, metas) <- prefilter (b :: Blund)
                                             pure (mb, bks, metas))
            .| CL.iterM applyHistorical
            .| CL.sinkNull

        -- Decide how to proceed.
        case (toList blunds) ^? _head . _1 . _Right . to headerHash of
             Nothing -> throwM (RestorationFinishUnreachable tgtHash startingPointWithinEpoch)
             Just lastH ->
                 if tgtHash == lastH then
                     finish
                   else nextHistoricalHash lastH >>= \case
                     Nothing  -> throwM (RestorationFinishUnreachable tgtHash lastH)
                     Just hh' -> restore genesisHash (epoch + 1, hh') timing'

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
