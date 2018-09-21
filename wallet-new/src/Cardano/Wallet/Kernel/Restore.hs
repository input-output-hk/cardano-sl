{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore
    ( restoreWallet
    , restoreKnownWallet
    ) where

import           Universum

import           Control.Concurrent.Async (async, cancel)
import           Control.Lens (at)
import           Data.Acid (update)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
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
                     RestoreHdWallet (..), dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockContext
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (CreateHdRootError)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     decryptAddress, keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     WalletRestorationProgress (..), addRestoration,
                     cancelRestoration, lookupRestorationInfo,
                     removeRestoration, restartRestoration, walletKeystore,
                     walletMeta, walletNode, wallets, wrpCurrentSlot,
                     wrpTargetSlot, wrpThroughput)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (Lock, LockContext (..),
                     NodeConstraints, NodeStateAdaptor, WithNodeState,
                     defaultGetSlotStart, filterUtxo, getCoreConfig,
                     getSecurityParameter, getSlotCount, mostRecentMainBlock,
                     withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock, UtxoWithAddrId, WalletKey,
                     prefilterBlock, prefilterUtxo', toHdAddressId,
                     toPrefilteredUtxo)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     WalletId (..), fromRawResolvedBlock, rawResolvedBlock,
                     rawResolvedBlockInputs, rawResolvedContext, rawTimestamp)
import           Cardano.Wallet.Kernel.Util.Core (utxoBalance)
import           Cardano.Wallet.Kernel.Wallets (createWalletHdRnd)

import           Pos.Chain.Block (Block, Blund, HeaderHash, Undo, mainBlockSlot,
                     undoTx)
import           Pos.Chain.Genesis (GenesisHash, configGenesisData,
                     configGenesisHash)
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxIn (..), TxOut (..), TxOutAux (..), Utxo,
                     genesisUtxo)
import           Pos.Core as Core (Address, BlockCount (..), Coin, SlotId,
                     flattenSlotId, getCurrentTimestamp, mkCoin,
                     unsafeIntegerToCoin)
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
              -> Core.Address
              -- ^ The stock address to use for the companion 'HdAccount'.
              -> HD.WalletName
              -> HD.AssuranceLevel
              -> EncryptedSecretKey
              -> IO (Either CreateHdRootError (HD.HdRoot, Coin))
restoreWallet pw hasSpendingPassword defaultCardanoAddress name assurance esk = do
    coreConfig <- getCoreConfig (pw ^. walletNode)
    walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo coreConfig wkey
    case walletInitInfo of
      WalletCreate utxos -> do
        root <- createWalletHdRnd pw hasSpendingPassword defaultCardanoAddress name assurance esk $
                \root defaultHdAccount defaultHdAddress ->
                      Left $ CreateHdWallet root defaultHdAccount defaultHdAddress utxos
        return $ fmap (, mkCoin 0) root
      WalletRestore utxos tgt -> do
          -- Create the wallet for restoration, deleting the wallet first if it
          -- already exists.
          mRoot <- createWalletHdRnd pw hasSpendingPassword defaultCardanoAddress name assurance esk $
                  \root defaultHdAccount defaultHdAddress ->
                      Right $ RestoreHdWallet root defaultHdAccount defaultHdAddress tgt utxos
          case mRoot of
              Left  err  -> return (Left err)
              Right root -> do
                  -- Start the restoration task.
                  beginRestoration pw wId prefilter root tgt (restart root)

                  -- Return the wallet's current balance.
                  let coins = unsafeIntegerToCoin
                            . utxoBalance
                            . M.unions
                            . M.elems
                            . fmap (\(cur, _gen, _addrs) -> cur)
                            $ utxos
                  return (Right (root, coins))

  where
    prefilter :: Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta])
    prefilter = mkPrefilter pw wId esk

    restart :: HD.HdRoot -> IO ()
    restart root = do
        coreConfig <- getCoreConfig (pw ^. walletNode)
        walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo coreConfig wkey
        case walletInitInfo of
            WalletCreate _utxos -> return ()
            WalletRestore _utxos tgt ->
                beginRestoration pw wId prefilter root tgt (restart root)

    wId    = WalletIdHdRnd (HD.eskToHdRootId esk)
    wkey   = (wId, keyToWalletDecrCredentials (KeyForRegular esk))


mkPrefilter :: Kernel.PassiveWallet
            -> WalletId
            -> EncryptedSecretKey
            -> Blund
            -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta])
mkPrefilter pw wId esk blund = blundToResolvedBlock (pw ^. walletNode) blund <&> \case
    Nothing -> (M.empty, [])
    Just rb -> prefilterBlock rb wId esk

-- | Begin a restoration for a wallet that is already known. This is used
-- to put an existing wallet back into a restoration state when something has
-- gone wrong.
restoreKnownWallet :: Kernel.PassiveWallet
                   -> HD.HdRootId
                   -> IO ()
restoreKnownWallet pw rootId = do
    let wId = WalletIdHdRnd rootId
    lookupRestorationInfo pw wId >>= \case
        -- Restart a pre-existing restoration
        Just wri -> do
            cancelRestoration  wri
            restartRestoration wri

        -- Start a new restoration of a seemingly up-to-date wallet.
        Nothing -> Keystore.lookup wId (pw ^. walletKeystore) >>= \case
            Nothing  -> return () -- TODO (@mn): raise an error
            Just esk -> do
                let prefilter = mkPrefilter pw wId esk
                    wkey = (wId, keyToWalletDecrCredentials (KeyForRegular esk))

                coreConfig <- getCoreConfig (pw ^. walletNode)
                db <- getWalletSnapshot pw
                case db ^. dbHdWallets . HD.hdWalletsRoots . at rootId of
                    Nothing   -> return () -- TODO (@mn): this really shouldn't happen
                    Just root ->
                      let restart =
                              withNodeState (pw ^. walletNode) (getWalletInitInfo coreConfig wkey) >>= \case
                                  WalletCreate  _utxos     -> return ()
                                  WalletRestore _utxos tgt ->
                                    beginRestoration pw wId prefilter root tgt restart
                      in restart

beginRestoration  :: Kernel.PassiveWallet
                  -> WalletId
                  -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                  -> HD.HdRoot
                  -> BlockContext
                  -> IO ()
                  -> IO ()
beginRestoration pw wId prefilter root tgt restart = do

    let tgtTip  = tgt ^. bcHash   . fromDb
        tgtSlot = tgt ^. bcSlotId . fromDb

    -- Set the wallet's restoration information
    slotCount <- getSlotCount (pw ^. walletNode)
    progress <- newIORef $ WalletRestorationProgress
                           { _wrpCurrentSlot = 0
                           , _wrpTargetSlot  = flattenSlotId slotCount tgtSlot
                           , _wrpThroughput  = MeasuredIn 0
                           }
    theTask <- newEmptyMVar

    let restoreInfo = WalletRestorationInfo
                      { _wriProgress = readIORef progress
                      , _wriCancel   = readMVar theTask >>= cancel
                      , _wriRestart  = restart
                      }

    addRestoration pw wId restoreInfo

    -- Begin restoring the wallet history in the background.
    restoreTask <- async $
        -- We are starting this async /from/ a thread that runs in response
        -- to a REST request. Linking the async to that REST request thread
        -- is pointless, because that thread will probably be long gone if
        -- an exception ever happens in the restoration worker. Therefore
        -- we just log any errors.
        catch (restoreWalletHistoryAsync pw
                                         (root ^. HD.hdRootId)
                                         prefilter
                                         progress
                                         (tgtTip, tgtSlot)) $ \(e :: SomeException) ->
              (pw ^. walletLogMessage) Error ("Exception during restoration: " <> show e)

    void $ swapMVar theTask restoreTask

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
      BlockContext

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
getWalletInitInfo coreConfig wKey@(wId, wdc) lock = do
    -- Find all of the current UTXO that this wallet owns.
    -- We lock the node state to be sure the tip header and the UTxO match
    (tipHeader, curUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])) <-
        fmap (second (fmap toPrefilteredUtxo . mergeUtxos)) $
          lock NotYetLocked $ \tip -> (tip, ) <$> filterUtxo isOurs

    -- Find genesis UTxO for this wallet
    let genUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])
        genUtxo = fmap toPrefilteredUtxo . snd $
                    prefilterUtxo' wKey
                                   (genesisUtxo $ configGenesisData coreConfig)

    -- Get the tip
    let gh = configGenesisHash coreConfig
    mTip <- mostRecentMainBlock gh tipHeader
    case mTip of
      Nothing  -> return (WalletCreate genUtxo)
      Just tip -> WalletRestore (mergeInfo curUtxo genUtxo) <$> mainBlockContext gh tip

  where

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
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> IORef WalletRestorationProgress
                          -> (HeaderHash, SlotId)
                          -> IO ()
restoreWalletHistoryAsync wallet rootId prefilter progress (tgtHash, tgtSlot) = do
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
                throughputUpdate = maybe identity (set wrpThroughput) blockPerSec
                slotId           = mb ^. mainBlockSlot
            modifyIORef progress ( (wrpCurrentSlot .~ flat slotId)
                                 . (wrpTargetSlot  .~ flat tgtSlot)
                                 . throughputUpdate )
            -- Store the TxMetas
            forM_ txMetas (putTxMeta (wallet ^. walletMeta))

        -- Decide how to proceed.
        if tgtHash == hh then
            finish
          else nextHistoricalHash hh >>= \case
            Nothing  -> throwM (RestorationFinishUnreachable tgtHash hh)
            Just hh' -> restore genesisHash hh' timing'

    -- TODO (@mn): probably should use some kind of bracket to ensure this cleanup happens.
    finish :: IO ()
    finish = do
        k <- getSecurityParameter (wallet ^. walletNode)
        update (wallet ^. wallets) $ RestorationComplete k rootId
        removeRestoration wallet wId

    -- Step forward to the successor of the given block.
    nextHistoricalHash :: HeaderHash -> IO (Maybe HeaderHash)
    nextHistoricalHash hh = withNode $ resolveForwardLink hh

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

{-------------------------------------------------------------------------------
  TODO (@mn): duplicated from Cardano.Wallet.WalletLayer.Kernel.Wallets
-------------------------------------------------------------------------------}

-- | The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
-- by the invariants established in the 'Blund'.
blundToResolvedBlock :: NodeStateAdaptor IO -> Blund -> IO (Maybe ResolvedBlock)
blundToResolvedBlock node (b,u) = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    case b of
      Left  _ebb      -> return Nothing
      Right mainBlock -> withNodeState node $ \_lock -> do
        ctxt  <- mainBlockContext genesisHash mainBlock
        mTime <- defaultGetSlotStart (mainBlock ^. mainBlockSlot)
        now   <- liftIO $ getCurrentTimestamp
        return $ Just $ fromRawResolvedBlock UnsafeRawResolvedBlock {
            rawResolvedBlock       = mainBlock
          , rawResolvedBlockInputs = map (map fromJust) $ undoTx u
          , rawTimestamp           = either (const now) identity mTime
          , rawResolvedContext     = ctxt
          }
