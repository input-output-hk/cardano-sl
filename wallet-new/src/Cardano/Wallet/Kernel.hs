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
  , applyBlock
  , applyBlocks
  , bracketPassiveWallet
  , createWalletHdRnd
  , init
  , walletLogMessage
  , walletPassive
    -- * The only effectful getter you will ever need
  , getWalletSnapshot
    -- * Pure getters acting on a DB snapshot
  , module Getters
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , NewPendingError
  ) where

import           Universum hiding (State, init)

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map.Strict as Map

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)
import           Data.Acid.Advanced (query', update')
import           Data.Acid.Memory (openMemoryState)

import           Cardano.Wallet.Kernel.Internal

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock, prefilterUtxo)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     CancelPending (..), CreateHdWallet (..), DB,
                     NewPending (..), NewPendingError, Snapshot (..), defDB)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.Spec (singletonPending)
import           Cardano.Wallet.Kernel.Submission (Cancelled, WalletSubmission,
                     addPending, defaultResubmitFunction, exponentialBackoff,
                     newWalletSubmission, tick)
import           Cardano.Wallet.Kernel.Submission.Worker (tickSubmissionLayer)
import           Cardano.Wallet.Kernel.Util (getCurrentTimestamp)

-- Handy re-export of the pure getters

import           Cardano.Wallet.Kernel.DB.Read as Getters

import           Pos.Core (ProtocolMagic, TxAux (..))
import           Pos.Core.Chrono (OldestFirst)
import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Txp (Utxo)


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
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage keystore f =
    bracket (liftIO $ openMemoryState defDB)
            (\_ -> return ())
            (\db ->
                bracket
                  (liftIO $ initPassiveWallet _walletLogMessage keystore db)
                  (\_ -> return ())
                  f)

{-------------------------------------------------------------------------------
  Manage the WalletESKs Map
-------------------------------------------------------------------------------}

-- | Insert an ESK, indexed by WalletId, to the Keystore.
insertWalletESK :: PassiveWallet -> WalletId -> EncryptedSecretKey -> IO ()
insertWalletESK pw wid esk
    = Keystore.insert wid esk (pw ^. walletKeystore)

withKeystore :: forall a. PassiveWallet -> (Keystore -> IO a) -> IO a
withKeystore pw action = action (pw ^. walletKeystore)

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> Keystore
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage keystore db = do
    return $ PassiveWallet logMessage keystore db

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = _walletLogMessage Info "Passive Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates an HD wallet with randomly generated addresses.
--
-- Prefilters the Utxo before passing it to the Acidstate update.

-- Adds an HdRoot and HdAccounts (which are discovered during prefiltering of utxo).
-- (In the case of empty utxo, no HdAccounts are created.)
-- May fail with CreateHdWalletError if the HdRootId already exists

-- The ESK is indexed by WalletId and added to the WalletESK map.
createWalletHdRnd :: PassiveWallet
                  -> HD.WalletName
                  -> HasSpendingPassword
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> Utxo
                  -> IO (Either HD.CreateHdRootError [HdAccountId])
createWalletHdRnd pw@PassiveWallet{..} name spendingPassword assuranceLevel esk utxo = do
    created <- InDb <$> getCurrentTimestamp
    let newRoot = HD.initHdRoot rootId name spendingPassword assuranceLevel created

    res <- update' _wallets $ CreateHdWallet newRoot utxoByAccount
    either (return . Left) insertESK res
    where
        utxoByAccount = prefilterUtxo rootId esk utxo
        accountIds    = Map.keys utxoByAccount

        rootId        = eskToHdRootId esk
        walletId      = WalletIdHdRnd rootId

        insertESK _arg = insertWalletESK pw walletId esk >> return (Right accountIds)

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each esk in the `WalletESK` map.
--   Return a unified Map of accountId and prefiltered blocks (representing multiple ESKs)
-- TODO(@uroboros/ryan) optimisation: we are prefiltering the block n times for n keys, change this to be a single pass
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO (Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b =
    withKeystore pw $ \ks ->
        (Map.unions . map prefilterBlock_) <$> Keystore.toList ks
    where
        prefilterBlock_ (wid,esk) = prefilterBlock wid esk b

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        blocksByAccount <- prefilterBlock' pw b
        -- apply block to all Accounts in all Wallets
        void $ update' _wallets $ ApplyBlock blocksByAccount

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO(@matt-noonan) this will be the responsibility of the worker thread (as part of CBR-243: Wallet restoration)
applyBlocks :: PassiveWallet
            -> OldestFirst [] ResolvedBlock
            -> IO ()
applyBlocks = mapM_ . applyBlock

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
            let txId = hash . taTx $ tx
            modifyMVar_ walletSubmission (return . addPending accountId (singletonPending txId tx))
            return $ Right ()

cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot
