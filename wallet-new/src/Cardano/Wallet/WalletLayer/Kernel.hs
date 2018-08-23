{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Maybe (fromJust)
import           System.Wlog (Severity (Debug))

import           Pos.Chain.Block (Blund, Undo (..), mainBlockSlot)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.BListener as Kernel
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     fromRawResolvedBlock)
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer (..))
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Active as Active
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Info as Info
import qualified Cardano.Wallet.WalletLayer.Kernel.Internal as Internal
import qualified Cardano.Wallet.WalletLayer.Kernel.Settings as Settings
import qualified Cardano.Wallet.WalletLayer.Kernel.Transactions as Transactions
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logFunction keystore rocksDB f = do
    Kernel.bracketPassiveWallet logFunction keystore rocksDB $ \w -> do
      let wai = Actions.WalletActionInterp
                 { Actions.applyBlocks = \blunds -> do
                    ls <- mapM (blundToResolvedBlock getTime)
                        (toList (getOldestFirst blunds))
                    let mp = catMaybes ls
                    Kernel.applyBlocks w (OldestFirst mp)
                 , Actions.switchToFork = \_ _ ->
                     logFunction Debug "<switchToFork>"
                 , Actions.emit = logFunction Debug }
      Actions.withWalletWorker wai $ \invoke -> do
         f (passiveWalletLayer w invoke) w
  where
    getTime :: Core.SlotId -> IO Core.Timestamp
    getTime n = do
        time <- rightToMaybe <$> getSlotStart rocksDB n
        defaultTime <- getCurrentTimestamp
        return $ fromMaybe defaultTime time

    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> STM ())
                       -> PassiveWalletLayer n
    passiveWalletLayer w invoke = PassiveWalletLayer
        { -- Operations that modify the wallet
          createWallet         = Wallets.createWallet         w
        , updateWallet         = Wallets.updateWallet         w
        , updateWalletPassword = Wallets.updateWalletPassword w
        , deleteWallet         = Wallets.deleteWallet         w
        , createAccount        = Accounts.createAccount       w
        , updateAccount        = Accounts.updateAccount       w
        , deleteAccount        = Accounts.deleteAccount       w
        , createAddress        = Addresses.createAddress      w
        , nextUpdate           = Internal.nextUpdate          w
        , applyUpdate          = Internal.applyUpdate         w
        , postponeUpdate       = Internal.postponeUpdate      w
        , resetWalletState     = Internal.resetWalletState    w
        , applyBlocks          = invokeIO . Actions.ApplyBlocks
        , rollbackBlocks       = invokeIO . Actions.RollbackBlocks . length

          -- Read-only operations
        , getWallets           =                   ro $ Wallets.getWallets
        , getWallet            = \wId           -> ro $ Wallets.getWallet wId
        , getUtxos             = \wId           -> ro $ Wallets.getWalletUtxos wId
        , getAccounts          = \wId           -> ro $ Accounts.getAccounts         wId
        , getAccount           = \wId acc       -> ro $ Accounts.getAccount          wId acc
        , getAccountBalance    = \wId acc       -> ro $ Accounts.getAccountBalance   wId acc
        , getAccountAddresses  = \wId acc rp fo -> ro $ Accounts.getAccountAddresses wId acc rp fo
        , getAddresses         = \rp            -> ro $ Addresses.getAddresses rp
        , validateAddress      = \txt           -> ro $ Addresses.validateAddress txt
        , getTransactions      = Transactions.getTransactions w
        , getTxFromMeta        = Transactions.toTransaction w
        , getNodeSettings      = Settings.getNodeSettings w
        }
      where
        -- Read-only operations
        ro :: (Kernel.DB -> x) -> n x
        ro g = g <$> liftIO (Kernel.getWalletSnapshot w)

        invokeIO :: forall m'. MonadIO m' => Actions.WalletAction Blund -> m' ()
        invokeIO = liftIO . STM.atomically . invoke


    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: (Core.SlotId -> IO Core.Timestamp) -> Blund -> IO (Maybe ResolvedBlock)
    blundToResolvedBlock getTimeBySlot (b,u) = do
        case b of
            Left _ ->  return Nothing
            Right mainBlock ->  do
                let slot = mainBlock ^. mainBlockSlot
                time <- getTimeBySlot slot
                return . Just $ fromRawResolvedBlock
                    (UnsafeRawResolvedBlock mainBlock spentOutputs' time)
        where
            spentOutputs' = map (map fromJust) $ undoTx u

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions, as it has the full
-- 'WalletDiffusion' layer in scope.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m, MonadIO n)
    => Core.ProtocolMagic
    -> PassiveWalletLayer n
    -> Kernel.PassiveWallet
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> Kernel.ActiveWallet -> m a) -> m a
bracketActiveWallet pm walletPassiveLayer passiveWallet walletDiffusion runActiveLayer =
    Kernel.bracketActiveWallet pm passiveWallet walletDiffusion $ \w -> do
        bracket
          (return (activeWalletLayer w))
          (\_ -> return ())
          (flip runActiveLayer w)
  where
    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer w = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer
        , pay                = Active.pay          w
        , estimateFees       = Active.estimateFees w
        , redeemAda          = Active.redeemAda    w
        , getNodeInfo        = Info.getNodeInfo    w
        }
