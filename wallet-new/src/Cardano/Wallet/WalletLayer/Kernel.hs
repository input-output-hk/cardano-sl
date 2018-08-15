{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Blund, Undo (..))
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Util.Trace.Named (TraceNamed, logDebug)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.BListener as Kernel
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     fromRawResolvedBlock)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer (..))
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Active as Active
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Transactions as Transactions
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => TraceNamed IO
    -> Keystore
    -> NodeStateAdaptor IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logTrace keystore rocksDB f =
    Kernel.bracketPassiveWallet logTrace keystore rocksDB $ \w -> do
      let wai = Actions.WalletActionInterp
                 { Actions.applyBlocks = \blunds ->
                     Kernel.applyBlocks w
                        (OldestFirst (mapMaybe blundToResolvedBlock
                           (toList (getOldestFirst blunds))))
                 , Actions.switchToFork = \_ _ ->
                     logDebug logTrace "<switchToFork>"
                 , Actions.emit = logDebug logTrace }
      Actions.withWalletWorker wai $ \invoke -> do
         f (passiveWalletLayer w invoke) w
  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> STM ())
                       -> PassiveWalletLayer n
    passiveWalletLayer w invoke = PassiveWalletLayer
        { -- Operations that modify the wallet
          _pwlCreateWallet         = Wallets.createWallet         w
        , _pwlUpdateWallet         = Wallets.updateWallet         w
        , _pwlUpdateWalletPassword = Wallets.updateWalletPassword w
        , _pwlDeleteWallet         = Wallets.deleteWallet         w
        , _pwlCreateAccount        = Accounts.createAccount       w
        , _pwlUpdateAccount        = Accounts.updateAccount       w
        , _pwlDeleteAccount        = Accounts.deleteAccount       w
        , _pwlCreateAddress        = Addresses.createAddress      w
        , _pwlApplyBlocks          = invokeIO . Actions.ApplyBlocks
        , _pwlRollbackBlocks       = invokeIO . Actions.RollbackBlocks
          -- Read-only operations
        , _pwlGetWallets           =             ro $ Wallets.getWallets
        , _pwlGetWallet            = \wId     -> ro $ Wallets.getWallet    wId
        , _pwlGetAccounts          = \wId     -> ro $ Accounts.getAccounts wId
        , _pwlGetAccount           = \wId acc -> ro $ Accounts.getAccount  wId acc
        , _pwlGetAddresses         = \rp      -> ro $ Addresses.getAddresses rp
        , _pwlValidateAddress      = \txt     -> ro $ Addresses.validateAddress txt
        , _pwlGetTransactions      = Transactions.getTransactions w
        , _pwlGetTxFromMeta        = Transactions.toTransaction w
        }
      where
        -- Read-only operations
        ro :: (Kernel.DB -> x) -> n x
        ro g = g <$> liftIO (Kernel.getWalletSnapshot w)

        invokeIO :: forall m'. MonadIO m' => Actions.WalletAction Blund -> m' ()
        invokeIO = liftIO . STM.atomically . invoke

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock $
              UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u
            rightToJust   = either (const Nothing) Just

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
        }
