{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore
    ( restoreWallet
    , stopAllRestorations
    ) where

import           Universum

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid (update)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                     getCurrentTime)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable

import qualified Prelude

import           Cardano.Wallet.API.Types.UnitOfMeasure
import           Cardano.Wallet.Kernel (walletLogMessage)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlock (..),
                     CreateHdWallet (..), RestorationComplete (..),
                     RestoreHdWallet (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (CreateHdRootError)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     decryptAddress, keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     walletMeta, walletNode, walletRestorationTask, wallets,
                     wriCancel, wriCurrentSlot, wriTargetSlot, wriThroughput)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (Lock, LockContext (..),
                     NodeConstraints, WithNodeState, filterUtxo, getCoreConfig,
                     getSecurityParameter, getSlotCount, mostRecentMainBlock,
                     withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock, UtxoWithAddrId, WalletKey,
                     prefilterUtxo', toHdAddressId, toPrefilteredUtxo)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (utxoBalance)
import           Cardano.Wallet.Kernel.Wallets (createWalletHdRnd)

import           Pos.Chain.Block (Block, Blund, HeaderHash, MainBlock, Undo,
                     headerHash, mainBlockSlot)
import           Pos.Chain.Genesis as Genesis (Config (..), GenesisHash)
import           Pos.Chain.Txp (TxIn (..), TxOut (..), TxOutAux (..), Utxo,
                     genesisUtxo)
import           Pos.Core (Address, BlockCount (..), Coin, SlotId,
                     flattenSlotId, mkCoin, unsafeIntegerToCoin)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.DB.Block (getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)
import           Pos.DB.Class (getBlock)
import           Pos.Util.Trace (Severity (Error))

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
-- NOTE: We pass in a fresh 'Address' which will be used to initialise the
-- companion 'HdAccount' this wallet will be created with. The reason why
-- we do this is that, if we were to use the 'PassPhrase' directly, it would
-- have been impossible for upstream code dealing with migrations to call
-- this function, as during migration time you don't have access to the
-- users' spending passwords.
-- During migration, instead, you can pick one of the @existing@ addresses
-- in the legacy wallet layer, and use it as input.
restoreWallet :: Kernel.PassiveWallet
              -> Bool
              -- ^ Did this wallet have a spending password set?
              -> Address
              -- ^ The stock address to use for the companion 'HdAccount'.
              -> HD.WalletName
              -> HD.AssuranceLevel
              -> EncryptedSecretKey
              -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
              -> IO (Either CreateHdRootError (HD.HdRoot, Coin))
restoreWallet pw hasSpendingPassword defaultCardanoAddress name assurance esk prefilter = do
    genesisConfig <- getCoreConfig (pw ^. walletNode)
    walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo genesisConfig wkey
    case walletInitInfo of
      WalletCreate utxos -> do
        root <- createWalletHdRnd pw hasSpendingPassword defaultCardanoAddress name assurance esk $
                \root defaultHdAccount defaultHdAddress ->
                      Left $ CreateHdWallet root defaultHdAccount defaultHdAddress utxos
        return $ fmap (, mkCoin 0) root
      WalletRestore utxos (tgtTip, tgtSlot) -> do
        -- Create the wallet
        mRoot <- createWalletHdRnd pw hasSpendingPassword defaultCardanoAddress name assurance esk $
                 \root defaultHdAccount defaultHdAddress ->
                       Right $ RestoreHdWallet root defaultHdAccount defaultHdAddress utxos
        case mRoot of
          Left  err  -> return (Left err)
          Right root -> do
            -- Set the wallet's restoration information
            slotCount <- getSlotCount (pw ^. walletNode)
            let restoreInfo = WalletRestorationInfo
                  { _wriCurrentSlot = 0
                  , _wriTargetSlot  = flattenSlotId slotCount tgtSlot
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
              catch (restoreWalletHistoryAsync pw (root ^. HD.hdRootId) tgtTip tgtSlot prefilter) $ \(e :: SomeException) ->
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
                  => Genesis.Config
                  -> WalletKey
                  -> Lock (WithNodeState IO)
                  -> WithNodeState IO WalletInitInfo
getWalletInitInfo genesisConfig wKey@(wId, wdc) lock = do
    -- Find all of the current UTXO that this wallet owns.
    -- We lock the node state to be sure the tip header and the UTxO match
    (tipHeader, curUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])) <-
        fmap (second (fmap toPrefilteredUtxo . mergeUtxos)) $
          lock NotYetLocked $ \tip -> (tip, ) <$> filterUtxo isOurs

    -- Find genesis UTxO for this wallet
    let genUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])
        genUtxo = fmap toPrefilteredUtxo . snd $
                    prefilterUtxo' wKey
                                   (genesisUtxo $ configGenesisData genesisConfig)

    -- Get the tip
    mTip <- mostRecentMainBlock (configGenesisHash genesisConfig) tipHeader
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
--
-- TODO: Think about what we should do if a 'RestorationException' is thrown.
restoreWalletHistoryAsync :: Kernel.PassiveWallet
                          -> HD.HdRootId
                          -> HeaderHash
                          -> SlotId
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> IO ()
restoreWalletHistoryAsync wallet rootId target tgtSlot prefilter = do
    genesisHash <- configGenesisHash <$> getCoreConfig (wallet ^. walletNode)
    -- 'getFirstGenesisBlockHash' is confusingly named: it returns the hash of
    -- the first block /after/ the genesis block.
    startingPoint <- withNode $ getFirstGenesisBlockHash genesisHash
    restore genesisHash startingPoint NoTimingData
  where
    wId :: WalletId
    wId = WalletIdHdRnd rootId

    -- Process the restoration of the block with the given 'HeaderHash'.
    restore :: GenesisHash -> HeaderHash -> TimingData -> IO ()
    restore genesisHash hh timing = do
        -- Updating the average rate every 5 blocks.
        (rate, timing') <- tickTiming 5 timing

        -- Update each account's historical checkpoints
        block <- getBlockOrThrow genesisHash hh

        -- Skip EBBs
        whenRight block $ \mb -> do
          -- Filter the blocks by account
          blund <- (Right mb, ) <$> getUndoOrThrow genesisHash hh
          (prefilteredBlocks, txMetas) <- prefilter blund

          -- Apply the block
          k    <- getSecurityParameter (wallet ^. walletNode)
          ctxt <- withNode $ mainBlockContext genesisHash mb
          mErr <- update (wallet ^. wallets) $
                    ApplyHistoricalBlock k ctxt prefilteredBlocks
          case mErr of
            Left err -> throwM $ RestorationApplyHistoricalBlockFailed err
            Right () -> return ()

          -- Update our progress
          slotCount <- getSlotCount (wallet ^. walletNode)
          let flat             = flattenSlotId slotCount
              blockPerSec      = MeasuredIn . BlockCount . perSecond <$> rate
              throughputUpdate = maybe identity (set wriThroughput) blockPerSec
              slotId           = mb ^. mainBlockSlot
          updateRestorationInfo wallet wId ( (wriCurrentSlot .~ flat slotId)
                                           . (wriTargetSlot  .~ flat tgtSlot)
                                           . throughputUpdate )

          -- Store the TxMetas
          forM_ txMetas (putTxMeta (wallet ^. walletMeta))

        -- Get the next block from the node and recurse.
        if target == hh then
          finish
        else
          nextBlock hh >>= \case
            Nothing      -> throwM $ RestorationFinishUnreachable target hh
            Just header' -> restore genesisHash header' timing'

    -- TODO (@mn): probably should use some kind of bracket to ensure this cleanup happens.
    finish :: IO ()
    finish = do
        k <- getSecurityParameter (wallet ^. walletNode)
        update (wallet ^. wallets) $ RestorationComplete k rootId
        modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.delete wId)

    -- Step forward to the successor of the given block.
    nextBlock :: HeaderHash -> IO (Maybe HeaderHash)
    nextBlock hh = withNode (resolveForwardLink hh)

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

    withNode :: forall a. (NodeConstraints => WithNodeState IO a) -> IO a
    withNode action = withNodeState (wallet ^. walletNode) (\_lock -> action)

-- Update the restoration information for a wallet.
updateRestorationInfo :: Kernel.PassiveWallet
                      -> WalletId
                      -> (WalletRestorationInfo -> WalletRestorationInfo)
                      -> IO ()
updateRestorationInfo wallet wId upd =
  modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.adjust upd wId)

-- | Clears the restoration state and stops and threads.
stopAllRestorations :: Kernel.PassiveWallet -> IO ()
stopAllRestorations pw = do
    modifyMVar_ (pw ^. walletRestorationTask) $ \mp -> do
      let vals = M.elems mp
      mapM_ _wriCancel vals
      return M.empty

{-------------------------------------------------------------------------------
  Timing information (for throughput calculations)
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception during restoration
data RestorationException =
    RestorationBlockNotFound HeaderHash
  | RestorationUndoNotFound HeaderHash
  | RestorationApplyHistoricalBlockFailed Spec.ApplyBlockFailed
  | RestorationFinishUnreachable HeaderHash HeaderHash

instance Buildable RestorationException where
    build (RestorationBlockNotFound hash) =
      bprint ("RestorationBlockNotFound " % build) hash
    build (RestorationUndoNotFound hash) =
      bprint ("RestorationUndoNotFound " % build) hash
    build (RestorationApplyHistoricalBlockFailed err) =
      bprint ("RestorationApplyHistoricalBlockFailed " % build) err
    build (RestorationFinishUnreachable target final) =
      bprint ("RestorationFinishUnreachable " % build % " " % build) target final

instance Show RestorationException where
    show = formatToString build

instance Exception RestorationException
