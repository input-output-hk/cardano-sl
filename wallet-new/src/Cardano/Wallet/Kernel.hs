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
    -- * Configuration
    , DatabaseMode(..)
    , DatabasePaths(..)
    , useDefaultPaths
    , defaultAcidStatePath
    , defaultSqlitePath
      -- ** Lenses
    , walletNode
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
import           Data.Acid (AcidState, createArchive, createCheckpoint,
                     openLocalStateFrom)
import           Data.Acid.Memory (openMemoryState)
import qualified Data.Map.Strict as Map

import           Pos.Chain.Txp (TxAux (..))
import           Pos.Core (ProtocolMagic)
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
                     addPendings, emptyWalletSubmission, tick)
import           Cardano.Wallet.Kernel.Submission.Worker (tickSubmissionLayer)

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | This type is used to configure the database location.
data DatabaseMode
    = UseInMemory
    -- ^ This constructor is used when you want to run the database in memory.
    -- This is useful for testing as it does not require a disk. The database
    -- will start out with the fresh, default, uninitialized state.
    | UseFilePath DatabasePaths
    -- ^ Load the databases from the given paths.

-- | A configuration type for specifying where to load the databases from.
data DatabasePaths
    = DatabasePaths
    { dbPathAcidState :: FilePath
    -- ^ The path for the @acid-state@ database.
    , dbPathMetadata  :: FilePath
    -- ^ This path is used for the SQLite database that contains the transaction
    -- metadata.
    } deriving (Eq, Show)

-- | Use the default paths on disk. See 'DatabasePaths' for more details.
useDefaultPaths :: DatabaseMode
useDefaultPaths =
    UseFilePath $ DatabasePaths defaultAcidStatePath defaultSqlitePath

defaultAcidStatePath :: FilePath
defaultAcidStatePath = "wallet-db-acid"

defaultSqlitePath :: FilePath
defaultSqlitePath = "./wallet-db-sqlite.sqlite3"

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet
    :: (MonadMask m, MonadIO m)
    => DatabaseMode
    -> (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet mode logMsg keystore node f =
    bracket (liftIO $ handlesOpen mode)
            (liftIO . handlesClose mode)
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
handlesOpen :: DatabaseMode -> IO WalletHandles
handlesOpen mode =
    case mode of
        UseInMemory -> do
            db <- openMemoryState defDB
            metadb <- openMetaDB ":memory:"
            migrateMetaDB metadb
            return $ Handles db metadb
        UseFilePath (DatabasePaths acidDb sqliteDb) -> do
            db <- openLocalStateFrom acidDb defDB
            metadb <- openMetaDB sqliteDb
            migrateMetaDB metadb
            return $ Handles db metadb

handlesClose :: DatabaseMode -> WalletHandles -> IO ()
handlesClose dbMode (Handles acidDb meta) = do
    closeMetaDB meta
    case dbMode of
        UseInMemory ->
            pure ()
        UseFilePath (DatabasePaths _ _) -> do
            createCheckpoint acidDb
            createArchive acidDb

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
            submission <- newMVar emptyWalletSubmission
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
            toSend <-
                modifyMVar submissionLayer $ \layer -> do
                    let (e, s, state') = tick layer
                    -- cancelPending is called in the MVar IO action so that we can reset the
                    -- state of the wallet using the MVar to block this thread.
                    -- If left outside, this thread could potentially
                    -- cancel Txs that were added after the reset.
                    unless (Map.null e) $
                        cancelPending walletPassive e
                    return (state', s)
            -- This can`t change the state of the wallet, so it`s left outside the MVar IO action.
            sendTransactions toSend
