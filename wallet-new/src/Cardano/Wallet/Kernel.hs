-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , bracketPassiveWallet
  , init
    -- ** Lenses
  , walletLogMessage
  , walletPassive
  , walletMeta
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  ) where

import           Universum hiding (State, init)

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar)
import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)
import qualified Data.Map.Strict as Map
import           System.Wlog (Severity (..))

import           Pos.Core (ProtocolMagic)
import           Pos.Core.Txp (TxAux (..))

import           Cardano.Wallet.Kernel.DB.AcidState (DB, defDB)
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.Pending (cancelPending)
import           Cardano.Wallet.Kernel.Submission (WalletSubmission,
                     defaultResubmitFunction, exponentialBackoff,
                     newWalletSubmission, tick)
import           Cardano.Wallet.Kernel.Submission.Worker (tickSubmissionLayer)

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

-- TODO(kde): this will be run with asynchronous exceptions masked.
-- and we should rethink if migrateMetaDB should happen here.
handlesOpen :: IO WalletHandles
handlesOpen = do
    db <- openMemoryState defDB
    metadb <- openMetaDB ":memory:" -- TODO: CBR-378
    migrateMetaDB metadb
    return $ Handles db metadb

handlesClose :: WalletHandles -> IO ()
handlesClose (Handles _ meta) = closeMetaDB meta

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
    submission <- newMVar (newWalletSubmission rho)
    return PassiveWallet {
          _walletLogMessage = logMessage
        , _walletKeystore   = keystore
        , _wallets          = hAcid
        , _walletMeta       = hMeta
        , _walletNode       = node
        , _walletSubmission = submission
        }
  where
    rho = defaultResubmitFunction (exponentialBackoff 255 1.25)

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = do
    _walletLogMessage Info $ "Passive Wallet kernel initialized."

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Initialize the active wallet
bracketActiveWallet :: (MonadMask m, MonadIO m)
                    => ProtocolMagic
                    -> PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletProtocolMagic
                    walletPassive
                    walletDiffusion
                    runActiveWallet = do
    submissionLayerTicker <- liftIO $ async $
      tickSubmissionLayer
        (walletPassive ^. walletLogMessage)
        (tickFunction (walletPassive ^. walletSubmission))
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
