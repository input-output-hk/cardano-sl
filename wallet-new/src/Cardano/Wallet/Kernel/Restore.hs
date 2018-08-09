{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore
    ( restoreWallet
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid (update)
import           Data.Conduit (mapOutputMaybe, runConduitRes, (.|))
import qualified Data.Conduit.List as Conduit
import qualified Data.Map as M
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                     getCurrentTime)
import           UnliftIO (MonadUnliftIO)

import           Cardano.Wallet.API.Types.UnitOfMeasure
import           Cardano.Wallet.Kernel (walletLogMessage)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.ChainState (ChainBrief (..),
                     ChainStateRestoration (..), getChainStateRestoration)
import           Cardano.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlock (..),
                     RestoreHdWallet (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdAddress)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (decryptAddress)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     walletMeta, walletNode, walletRestorationTask, wallets,
                     wriCancel, wriCurrentSlot, wriTargetSlot, wriThroughput)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (LockContext (..),
                     NodeConstraints, WithNodeState, getSecurityParameter,
                     getSlotCount, withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock, WalletKey,
                     toHdAddressId)
import           Cardano.Wallet.Kernel.Types (WalletId)
import           Cardano.Wallet.Kernel.Util.Core (utxoBalance)

import           Pos.Chain.Block (Blund, HeaderHash, headerHash, mainBlockSlot)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Address, BlockCount (..), Coin, SlotId,
                     flattenSlotIdExplicit, mkCoin)
import           Pos.Core.Txp (toaOut, txOutAddress)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Block (getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)
import           Pos.DB.Class (getBlock)
import           Pos.DB.Txp.Utxo (utxoSource)
import           Pos.Util.Trace (Severity (Error, Debug))
import           Pos.Wallet.Web.State (WAddressMeta (..))

-- | Scan the node's current UTXO set for any that belong to this wallet. Use them
--   to update the current checkpoint's UTXO set, and return the total 'Coin' value
--   of the UTXO belonging to this wallet. At the same time, kick off a background
--   thread that will asynchronously restore the wallet history.
restoreWallet :: Kernel.PassiveWallet
              -> HD.HdRoot
              -> WalletKey
              -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
              -> IO Coin
restoreWallet wallet hdRoot (wId, wdc) prefilter = do
    -- Find all of the current UTXO that this wallet owns.
    mine <- withNodeState (wallet ^. walletNode) (\_lock -> filterMyUtxo)

    -- Create the wallet and populate the accounts we've seen.
    let db = wallet ^. wallets
        utxoByAcct = map (\(u, a, wam) ->
                                 let hdAddr = initHdAddress (toHdAddressId wId wam) (InDb a)
                                     hdAddrId = hdAddr ^. HD.hdAddressId
                                 in (hdAddr ^. HD.hdAddressAccountId, (u, [(hdAddrId, a)]))) mine

    csr <- getChainStateRestoration (wallet ^. walletNode) NotYetLocked
    whenJust (csrCurrent csr) $ \brief -> do
        -- Create the wallet
        let tgtTip  = cbTip brief
            tgtSlot = cbSlotId brief
        void $ update db (RestoreHdWallet hdRoot tgtSlot (M.fromListWith (<>) utxoByAcct))

        -- Set the wallet's restoration information
        slotCount <- getSlotCount (wallet ^. walletNode)
        let restoreInfo = WalletRestorationInfo
              { _wriCurrentSlot = 0
              , _wriTargetSlot  = flattenSlotIdExplicit slotCount (cbSlotId brief)
              , _wriThroughput  = MeasuredIn 0
              , _wriCancel      = return ()
              }
        modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.insert wId restoreInfo)

        -- Begin restoring the wallet history in the background.
        restoreTask <- async $
          -- We are starting this async /from/ a thread that runs in response
          -- to a REST request. Linking the async to that REST request thread
          -- is pointless, because that thread will probably be long gone if
          -- an exception ever happens in the restoration worker. Therefore
          -- we just log any errors.
          catch (restoreWalletHistoryAsync wallet wId tgtTip tgtSlot prefilter) $ \(e :: SomeException) ->
            (wallet ^. walletLogMessage) Error ("Exception during restoration: " <> show e)

        -- Set up the cancellation action
        updateRestorationInfo wallet wId (wriCancel .~ cancel restoreTask)

    -- Return the wallet's current balance.
    let myUtxo  = foldr (\(u,_,_) -> M.union u) M.empty mine
    return (mkCoin . fromIntegral . utxoBalance $ myUtxo)

  where
    txoAddr = txOutAddress . toaOut . snd

    filterMyUtxo :: (MonadDBRead m, MonadUnliftIO m) => m [(Utxo, Address, WAddressMeta)]
    filterMyUtxo = runConduitRes
         $ mapOutputMaybe (\u -> let a = txoAddr u
                                     wam = decryptAddress wdc a
                                 in (M.fromList [u],a,) <$> wam) utxoSource
        .| Conduit.fold (flip (:)) []

-- | Restore a wallet's transaction history.
restoreWalletHistoryAsync :: Kernel.PassiveWallet
                          -> WalletId
                          -> HeaderHash
                          -> SlotId
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> IO ()
restoreWalletHistoryAsync wallet wId target tgtSlot prefilter = do

    say "sleeping 10 seconds before starting restoration, so that the tip has a chance to move"
    pause 10

    -- TODO (@mn): should filter genesis utxo for the first wallet
    withNode (getFirstGenesisBlockHash >>= getBlock) >>= \case
        Nothing  -> say "failed to find genesis block's successor!!" >> finish
        Just gbs -> restore (headerHash gbs) NoTimingData

  where

    pause = when True . threadDelay . (* 1000000)

    say = (wallet ^. walletLogMessage) Debug . ("mnoonan: " <>)

    -- Process the restoration of the block with the given 'HeaderHash'.
    -- The (UTCTime, Int) pair is used t
    restore :: HeaderHash -> TimingData -> IO ()
    restore hh timing = do

        -- Increment our timing counter, producing an average rate
        -- every 5 blocks.
        (rate, timing') <- tickTiming 5 timing

        -- Update each account's historical checkpoints
        (block, undo) <- withNode ((,) <$> getBlock hh <*> getUndo hh)

        whenJust ((,) <$> block <*> undo) $ \blund ->
          whenRight (fst blund) $ \mb -> do

            -- Gather the information we will need to decide if we are within K blocks of the tip.
            slotCount <- getSlotCount (wallet ^. walletNode)
            let flat = flattenSlotIdExplicit slotCount

            -- Filter the blocks by account
            (prefilteredBlocks, txMetas) <- prefilter blund

            let slotId = mb ^. mainBlockSlot
            k <- getSecurityParameter (wallet ^. walletNode)
            update (wallet ^. wallets)
                   (ApplyHistoricalBlock k (InDb slotId) slotCount prefilteredBlocks)

            -- Update our progress
            let blockPerSec = MeasuredIn . BlockCount . perSecond <$> rate
                throughputUpdate = maybe identity (set wriThroughput) blockPerSec

            updateRestorationInfo wallet wId ( (wriCurrentSlot .~ flat slotId)
                                             . (wriTargetSlot  .~ flat tgtSlot)
                                             . throughputUpdate )

            -- Store the TxMetas
            forM_ txMetas (putTxMeta (wallet ^. walletMeta))

        -- MN TEMPORARY: slow your roll! Add an artificial 5 second delay whenever
        -- the throughput rate was updated, so we can look for it in the `wallets`
        -- endpoint results.
        case rate of
            Nothing -> return ()
            Just _  -> pause 5

        -- Get the next block from the node and recurse.
        if target == hh
          then say "made it to target!" >> finish
          else nextBlock hh >>= \case
            Nothing      -> say "failed to find next block!!" >> finish
            Just header' -> restore header' timing'

    -- Step forward to the successor of the given block.
    nextBlock :: HeaderHash -> IO (Maybe HeaderHash)
    nextBlock hh = withNode (resolveForwardLink hh)

    -- TODO (@mn): probably should use some kind of bracket to ensure this cleanup happens.
    finish :: IO ()
    finish = modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.delete wId)

    withNode :: forall a. (NodeConstraints => WithNodeState IO a) -> IO a
    withNode action = withNodeState (wallet ^. walletNode) (\_lock -> action)

-- Update the restoration information for a wallet.
updateRestorationInfo :: Kernel.PassiveWallet
                      -> WalletId
                      -> (WalletRestorationInfo -> WalletRestorationInfo)
                      -> IO ()
updateRestorationInfo wallet wId upd =
  modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.adjust upd wId)

-- | Keep track of how many events have happened since a given start time.
data TimingData
  = NoTimingData
  | Timing Integer UTCTime

-- | A rate, represented as an event count over a time interval.
data Rate = Rate Integer NominalDiffTime

-- | Log an event; once k' events have been seen, return the event rate
-- and start the count over again.
tickTiming :: Integer -> TimingData -> IO (Maybe Rate, TimingData)
tickTiming _  NoTimingData     = (Nothing,) . Timing 0 <$> getCurrentTime
tickTiming k' (Timing k start)
  | k == k' = do
        now <- getCurrentTime
        let rate = Rate k (now `diffUTCTime` start)
        return (Just rate, Timing 0 now)
  | otherwise = return (Nothing, Timing (k + 1) start)

-- | Convert a rate to a number of events per second.
perSecond :: Rate -> Word64
perSecond (Rate n dt) = fromInteger $ round (toRational n / toRational dt)
