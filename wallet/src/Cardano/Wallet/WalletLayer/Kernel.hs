{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum hiding (for_)

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.List.NonEmpty as NE

import           Data.Foldable (for_)
import           Formatting ((%))
import qualified Formatting as F

import           Pos.Chain.Block (Blund, blockHeader, headerHash, prevBlockL)
import           Pos.Chain.Genesis (Config (..))
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.InjectFail (FInjects)
import           Pos.Util.Wlog (Severity (Debug, Warning))

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.BListener as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet (hdAccountRestorationState,
                     hdRootId, hdWalletsRoots)
import qualified Cardano.Wallet.Kernel.DB.Read as Kernel
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor
import           Cardano.Wallet.Kernel.ProtocolParameters
import qualified Cardano.Wallet.Kernel.Read as Kernel
import qualified Cardano.Wallet.Kernel.Restore as Kernel
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
    :: forall m n a. (MonadIO n, MonadUnliftIO m, MonadMask m)
    => ProtocolMagic
    -> Kernel.DatabaseMode
    -> (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> ProtocolParameterAdaptor
    -> FInjects IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet pm mode logFunction keystore node pp fInjects f = do
    Kernel.bracketPassiveWallet pm mode logFunction keystore node pp fInjects $ \w -> do

      -- For each wallet in a restoration state, re-start the background
      -- restoration tasks.
      liftIO $ do
          snapshot <- Kernel.getWalletSnapshot w
          let wallets = snapshot ^. dbHdWallets . hdWalletsRoots
          for_ wallets $ \root -> do
              let accts      = Kernel.accountsByRootId snapshot (root ^. hdRootId)
                  restoring  = IxSet.findWithEvidence hdAccountRestorationState accts

              whenJust restoring $ \(src, tgt) -> do
                  (w ^. Kernel.walletLogMessage) Warning $
                      F.sformat ("bracketPassiveWallet: continuing restoration of " %
                       F.build %
                       " from checkpoint " % F.build %
                       " with target "     % F.build)
                       (root ^. hdRootId) (maybe "(genesis)" pretty src) (pretty tgt)
                  Kernel.continueRestoration w root src tgt

      -- Start the wallet worker
      let wai = Actions.WalletActionInterp
                 { Actions.applyBlocks = \blunds -> do
                    ls <- mapM (Wallets.blundToResolvedBlock node)
                        (toList (getOldestFirst blunds))
                    let mp = catMaybes ls
                    mapM_ (Kernel.applyBlock w) mp

                 , Actions.switchToFork = \_ (OldestFirst blunds) -> do
                     -- Get the hash of the last main block before this fork.
                     let almostOldest = fst (NE.head blunds)
                     gh     <- configGenesisHash <$> getCoreConfig node
                     oldest <- withNodeState node $ \_lock ->
                                 mostRecentMainBlock gh
                                   (almostOldest ^. blockHeader . prevBlockL)

                     bs <- catMaybes <$> mapM (Wallets.blundToResolvedBlock node)
                                             (NE.toList blunds)

                     Kernel.switchToFork w (headerHash <$> oldest) bs

                 , Actions.emit = logFunction Debug
                 }
      Actions.withWalletWorker wai $ \invoke -> do
         f (passiveWalletLayer w invoke) w

  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> STM ())
                       -> PassiveWalletLayer n
    passiveWalletLayer w invoke = PassiveWalletLayer
        { -- Operations that modify the wallet
          createWallet         = Wallets.createWallet         w
        , createEosWallet      = Wallets.createEosWallet      w
        , updateWallet         = Wallets.updateWallet         w
        , updateWalletPassword = Wallets.updateWalletPassword w
        , deleteWallet         = Wallets.deleteWallet         w
        , deleteEosWallet      = Wallets.deleteEosWallet      w
        , createAccount        = Accounts.createAccount       w
        , updateAccount        = Accounts.updateAccount       w
        , deleteAccount        = Accounts.deleteAccount       w
        , createAddress        = Addresses.createAddress      w
        , addUpdate            = Internal.addUpdate           w
        , nextUpdate           = Internal.nextUpdate          w
        , applyUpdate          = Internal.applyUpdate         w
        , postponeUpdate       = Internal.postponeUpdate      w
        , waitForUpdate        = Internal.waitForUpdate       w
        , resetWalletState     = Internal.resetWalletState    w
        , importWallet         = Internal.importWallet        w
        , applyBlocks          = invokeIO . Actions.ApplyBlocks
        , rollbackBlocks       = invokeIO . Actions.RollbackBlocks . length

          -- Read-only operations
        , getWallets           =                   join (ro $ Wallets.getWallets w)
        , getWallet            = \wId           -> join (ro $ Wallets.getWallet w wId)
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

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions, as it has the full
-- 'WalletDiffusion' layer in scope.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m, MonadIO n)
    => PassiveWalletLayer n
    -> Kernel.PassiveWallet
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> Kernel.ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassiveLayer passiveWallet walletDiffusion runActiveLayer =
    Kernel.bracketActiveWallet passiveWallet walletDiffusion $ \w -> do
        bracket
          (return (activeWalletLayer w))
          (\_ -> return ())
          (flip runActiveLayer w)
  where
    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer w = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer
        , pay                = Active.pay              w
        , estimateFees       = Active.estimateFees     w
        , createUnsignedTx   = Active.createUnsignedTx w
        , submitSignedTx     = Active.submitSignedTx   w
        , redeemAda          = Active.redeemAda        w
        , getNodeInfo        = Info.getNodeInfo        w
        }
