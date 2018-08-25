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
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                     getCurrentTime)

import           Cardano.Wallet.API.Types.UnitOfMeasure
import           Cardano.Wallet.Kernel (walletLogMessage)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlock (..),
                     CreateHdWallet (..), RestoreHdWallet (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (CreateHdRootError)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     decryptAddress, keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     walletMeta, walletNode, walletRestorationTask, wallets,
                     wriCancel, wriCurrentSlot, wriTargetSlot, wriThroughput)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (Lock, LockContext (..),
                     NodeConstraints, WithNodeState, filterUtxo,
                     getSecurityParameter, getSlotCount, mostRecentMainBlock,
                     withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock, UtxoWithAddrId, WalletKey,
                     prefilterUtxo', toHdAddressId, toPrefilteredUtxo)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (utxoBalance)
import           Cardano.Wallet.Kernel.Wallets (createWalletHdRnd)

import           Pos.Chain.Block (Blund, HeaderHash, MainBlock, headerHash,
                     mainBlockSlot)
import           Pos.Chain.Txp (GenesisUtxo (..), Utxo, genesisUtxo)
import           Pos.Core (BlockCount (..), Coin, SlotId, flattenSlotIdExplicit,
                     mkCoin, unsafeIntegerToCoin)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.DB.Block (getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)
import           Pos.DB.Class (getBlock)
import           Pos.Util.Trace (Severity (Debug, Error))

-- | Restore a wallet
--
-- NOTE: The key for the wallet must already have been added to the keystore.
--
-- Scan the node's current UTXO set for any that belong to this wallet. Use them
-- to update the current checkpoint's UTXO set, and return the total 'Coin'
-- value of the UTXO belonging to this wallet. At the same time, kick off a
-- background thread that will asynchronously restore the wallet history.
--
-- Wallet initialization parameters match those of 'createWalletHdRnd'
restoreWallet :: Kernel.PassiveWallet
              -> Bool -- ^ Spending password
              -> HD.WalletName
              -> HD.AssuranceLevel
              -> EncryptedSecretKey
              -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
              -> IO (Either CreateHdRootError (HD.HdRoot, Coin))
restoreWallet pw spendingPass name assurance esk prefilter = do
    walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo wkey
    case walletInitInfo of
      WalletCreate utxos -> do
        root <- createWalletHdRnd pw spendingPass name assurance esk $ \root ->
                  Left $ CreateHdWallet root utxos
        return $ fmap (, mkCoin 0) root
      WalletRestore utxos (tgtTip, tgtSlot) -> do
        -- Create the wallet
        mRoot <- createWalletHdRnd pw spendingPass name assurance esk $ \root ->
                   Right $ RestoreHdWallet root tgtSlot utxos
        case mRoot of
          Left  err  -> return (Left err)
          Right root -> do
            -- Set the wallet's restoration information
            slotCount <- getSlotCount (pw ^. walletNode)
            let restoreInfo = WalletRestorationInfo
                  { _wriCurrentSlot = 0
                  , _wriTargetSlot  = flattenSlotIdExplicit slotCount tgtSlot
                  , _wriThroughput  = MeasuredIn 0
                  , _wriCancel      = return ()
                  }
            modifyMVar_ (pw ^. walletRestorationTask) (pure . M.insert wId restoreInfo)

            -- Begin restoring the wallet history in the background.
            restoreTask <- async $
              -- We are starting this async /from/ a thread that runs in response
              -- to a REST request. Linking the async to that REST request thread
              -- is pointless, because that thread will probably be long gone if
              -- an exception ever happens in the restoration worker. Therefore
              -- we just log any errors.
              catch (restoreWalletHistoryAsync pw wId tgtTip tgtSlot prefilter) $ \(e :: SomeException) ->
                (pw ^. walletLogMessage) Error ("Exception during restoration: " <> show e)

            -- Set up the cancellation action
            updateRestorationInfo pw wId (wriCancel .~ cancel restoreTask)

            -- Return the wallet's current balance.
            let balance = unsafeIntegerToCoin
                        . utxoBalance
                        . M.unions
                        . M.elems
                        . fmap (\(cur, _gen, _addrs) -> cur)
                        $ utxos
            return $ Right (root, balance)
  where
    rootId = HD.eskToHdRootId esk
    wId    = WalletIdHdRnd rootId
    wkey   = (wId, keyToWalletDecrCredentials (KeyForRegular esk))

-- | Information we need to start the restoration process
data WalletInitInfo =
    -- | Create the wallet, without actually restoring
    --
    -- This is used only when the chain has no main blocks yet. We record
    -- the only the genesis UTxO for the wallet, and any addresses we found.
    WalletCreate
      (Map HD.HdAccountId (Utxo, [AddrWithId]))

    -- | Restore the wallet
    --
    -- We record the current and genesis UTxO, as well as some information
    -- about the most recent main block on the chain.
  | WalletRestore
      (Map HD.HdAccountId (Utxo, Utxo, [AddrWithId]))
      (HeaderHash, SlotId)

-- | Query the underlying node for the info we need to restore a wallet
--
-- We return the current and genesis UTxO for this wallet, as well some
-- information about the tip of the blockchain (provided the blockchain
-- isn't empty).
getWalletInitInfo :: NodeConstraints
                  => WalletKey
                  -> Lock (WithNodeState IO)
                  -> WithNodeState IO WalletInitInfo
getWalletInitInfo wKey@(wId, wdc) lock = do
    -- Find all of the current UTXO that this wallet owns.
    -- We lock the node state to be sure the tip header and the UTxO match
    (tipHeader, curUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])) <-
        fmap (second (fmap toPrefilteredUtxo . mergeUtxos)) $
          lock NotYetLocked $ \tip -> (tip, ) <$> filterUtxo isOurs

    -- Find genesis UTxO for this wallet
    let genUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])
        genUtxo = fmap toPrefilteredUtxo . snd $
                    prefilterUtxo' wKey (unGenesisUtxo genesisUtxo)

    -- Get the tip
    mTip <- mostRecentMainBlock tipHeader
    return $ case mTip of
      Nothing  -> WalletCreate genUtxo
      Just tip -> WalletRestore (mergeInfo curUtxo genUtxo) (tipInfo tip)
  where
    tipInfo :: MainBlock -> (HeaderHash, SlotId)
    tipInfo mb = (headerHash mb, mb ^. mainBlockSlot)

    mergeInfo :: (Monoid cur, Monoid gen)
              => Map HD.HdAccountId (cur, [AddrWithId])
              -> Map HD.HdAccountId (gen, [AddrWithId])
              -> Map HD.HdAccountId (cur, gen, [AddrWithId])
    mergeInfo = M.merge
        (M.mapMaybeMissing     $ \_ (c, as) -> Just (c, mempty, as))
        (M.mapMaybeMissing     $ \_ (g, as) -> Just (mempty, g, as))
        (M.zipWithMaybeMatched $ \_ (c, as) (g, as') -> Just (c, g, as ++ as'))

    mergeUtxos :: [(HD.HdAccountId, UtxoWithAddrId)]
               -> Map HD.HdAccountId UtxoWithAddrId
    mergeUtxos = M.fromListWith M.union

    isOurs :: (TxIn, TxOutAux) -> Maybe (HD.HdAccountId, UtxoWithAddrId)
    isOurs (inp, out@(TxOutAux (TxOut addr _))) = do
        wam <- decryptAddress wdc addr
        let addrId = toHdAddressId wId wam
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))

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
