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
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)
import qualified Data.Map.Strict as Map

import           Pos.Core (ProtocolMagic)
import           Pos.Core.Txp (TxAux (..))
import           Pos.Util.Wlog (Severity (..))

import           Cardano.Wallet.Kernel.DB.AcidState (DB, defDB)
import           Cardano.Wallet.Kernel.DB.Read (pendingByAccount)
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.Pending (cancelPending)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Submission (WalletSubmission,
                     addPendings, defaultResubmitFunction, exponentialBackoff,
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

-- | Initialise Passive Wallet
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> Keystore
                  -> WalletHandles
                  -> NodeStateAdaptor IO
                  -> IO PassiveWallet
initPassiveWallet logMessage keystore handles node = do
    pw <- preparePassiveWallet
    initSubmission pw
    return pw
    where
        -- | Prepare Passive Wallet for initialisation.
        -- NOTE: the Submission Layer is not initialised yet since that would require
        -- access to the PassiveWallet state
        preparePassiveWallet :: IO PassiveWallet
        preparePassiveWallet = do
            submission <- newMVar (newWalletSubmission rho)
            restore    <- newMVar Map.empty
            return PassiveWallet {
                  _walletLogMessage      = logMessage
                , _walletKeystore        = keystore
                , _wallets               = hAcid handles
                , _walletMeta            = hMeta handles
                , _walletNode            = node
                , _walletSubmission      = submission
                , _walletRestorationTask = restore
                }
          where
            rho = defaultResubmitFunction (exponentialBackoff 255 1.25)

        -- | Since the submission layer state is not persisted, we need to initialise
        -- the submission layer with all pending transactions present in the wallet state.
        initSubmission :: PassiveWallet -> IO ()
        initSubmission pw_  = do
            pendings <- pendingByAccount <$> getWalletSnapshot pw_
            modifyMVar_ (_walletSubmission pw_) $
                return . addPendings pendings

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
