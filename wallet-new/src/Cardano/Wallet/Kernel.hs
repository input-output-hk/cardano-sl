-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , DB -- opaque
  , WalletId
  , bracketPassiveWallet
  , init
  , walletLogMessage
  , walletPassive
    -- ** Respond to block chain events
  , applyBlock
  , applyBlocks
  , switchToFork
    -- *** Testing
  , observableRollbackUseInTestsOnly
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters

  , walletMeta
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , newForeign
  , NewPendingError
  ) where

import           Universum hiding (State, init)

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import           Data.Acid (AcidState)
import           Data.Acid.Advanced (query', update')
import           Data.Acid.Memory (openMemoryState)
import qualified Data.Map.Strict as Map
import           System.Wlog (Severity (..))

import           Pos.Core (ProtocolMagic, SlotId)
import           Pos.Core.Chrono (OldestFirst)
import           Pos.Core.Txp (TxAux (..))
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     CancelPending (..), DB, NewForeign (..), NewForeignError,
                     NewPending (..), NewPendingError,
                     ObservableRollbackUseInTestsOnly (..),
                     RollbackDuringRestoration, Snapshot (..),
                     SwitchToFork (..), defDB)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbSlotId)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Submission (Cancelled, WalletSubmission,
                     addPending, defaultResubmitFunction, exponentialBackoff,
                     newWalletSubmission, tick)
import           Cardano.Wallet.Kernel.Submission.Worker (tickSubmissionLayer)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> Keystore
                     -> NodeStateAdaptor IO
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet logMsg keystore node f =
    bracket (liftIO $ handlesOpen)
            (liftIO . handlesClose)
            (\ handles ->
                bracket
                  (liftIO $ initPassiveWallet logMsg keystore handles node)
                  (\_ -> return ())
                  f)


data WalletHandles = Handles {
    hAcid :: AcidState DB,
    hMeta :: MetaDBHandle
}

handlesOpen :: IO WalletHandles
handlesOpen = do
    db <- openMemoryState defDB
    metadb <- openMetaDB ":memory:" -- TODO: CBR-378
    migrateMetaDB metadb            -- TODO: this will be run with asynchronous exceptions masked.
    return $ Handles db metadb

handlesClose :: WalletHandles -> IO ()
handlesClose (Handles _ meta) = closeMetaDB meta

{-------------------------------------------------------------------------------
  Manage the Wallet's ESKs
-------------------------------------------------------------------------------}

withKeystore :: forall a. PassiveWallet -> (Keystore -> IO a) -> IO a
withKeystore pw action = action (pw ^. walletKeystore)

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> Keystore
                  -> WalletHandles
                  -> NodeStateAdaptor IO
                  -> IO PassiveWallet
initPassiveWallet logMessage keystore Handles{..} node = do
    return $ PassiveWallet logMessage keystore hAcid hMeta node

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = do
    _walletLogMessage Info $ "Passive Wallet kernel initialized."

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each account.
--
-- TODO: Improve performance (CBR-379)
-- TODO: This not be defined in terms of they keystore (CBR-341)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO (SlotId, Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b =
    withKeystore pw $ \ks -> aux <$> Keystore.toList ks
  where
    aux :: [(WalletId, EncryptedSecretKey)]
        -> (SlotId, Map HdAccountId PrefilteredBlock)
    aux ws = (
        b ^. rbSlotId
      , Map.unions $ map (uncurry (prefilterBlock b)) ws
      )

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        (slotId, blocksByAccount) <- prefilterBlock' pw b
        -- apply block to all Accounts in all Wallets
        update' _wallets $ ApplyBlock (InDb slotId) blocksByAccount

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO(@matt-noonan) this will be the responsibility of the worker thread (as part of CBR-243: Wallet restoration)
applyBlocks :: PassiveWallet
            -> OldestFirst [] ResolvedBlock
            -> IO ()
applyBlocks = mapM_ . applyBlock

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either RollbackDuringRestoration ())
switchToFork pw@PassiveWallet{..} n bs = do
    blockssByAccount <- mapM (prefilterBlock' pw) bs
    update' _wallets $ SwitchToFork n blockssByAccount

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either RollbackDuringRestoration ())
observableRollbackUseInTestsOnly PassiveWallet{..} =
    update' _wallets $ ObservableRollbackUseInTestsOnly

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Initialize the active wallet
bracketActiveWallet :: (MonadMask m, MonadIO m)
                    => ProtocolMagic
                    -> PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletProtocolMagic walletPassive walletDiffusion runActiveWallet = do
    let logMsg = _walletLogMessage walletPassive
    let rho = defaultResubmitFunction (exponentialBackoff 255 1.25)
    walletSubmission <- newMVar (newWalletSubmission rho)
    submissionLayerTicker <-
        liftIO $ async
               $ tickSubmissionLayer logMsg (tickFunction walletSubmission)
    bracket
      (return ActiveWallet{..})
      (\_ -> liftIO $ do
                 (_walletLogMessage walletPassive) Error "stopping the wallet submission layer..."
                 cancel submissionLayerTicker
      )
      runActiveWallet
    where
        -- NOTE(adn) We might want to discuss diffusion layer throttling
        -- with Alex & Duncan.
        -- By default the diffusion layer should correctly throttle and debounce
        -- requests, but we might want in the future to adopt more sophisticated
        -- strategies.
        sendTransactions :: [TxAux] -> IO ()
        sendTransactions [] = return ()
        sendTransactions (tx:txs) = do
            void $ (walletSendTx walletDiffusion) tx
            sendTransactions txs

        tickFunction :: MVar WalletSubmission -> IO ()
        tickFunction submissionLayer = do
            (cancelled, toSend) <-
                modifyMVar submissionLayer $ \layer -> do
                    let (e, s, state') = tick layer
                    return (state', (e,s))
            unless (Map.null cancelled) $
                cancelPending walletPassive cancelled
            sendTransactions toSend

-- | Submit a new pending transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
newPending :: ActiveWallet -> HdAccountId -> TxAux -> IO (Either NewPendingError ())
newPending ActiveWallet{..} accountId tx = do
    res <- update' (walletPassive ^. wallets) $ NewPending accountId (InDb tx)
    case res of
        Left e -> return (Left e)
        Right () -> do
            modifyMVar_ walletSubmission (return . addPending accountId (Pending.singleton tx))
            return $ Right ()

-- | Submit new foreign transaction
--
-- A foreign transaction is a transaction that transfers funds from /another/
-- wallet to this one.
newForeign :: ActiveWallet -> HdAccountId -> TxAux -> IO (Either NewForeignError ())
newForeign ActiveWallet{..} accountId tx = do
    res <- update' (walletPassive ^. wallets) $ NewForeign accountId (InDb tx)
    case res of
        Left e -> return (Left e)
        Right () -> do
            modifyMVar_ walletSubmission (return . addPending accountId (Pending.singleton tx))
            return $ Right ()

cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot
