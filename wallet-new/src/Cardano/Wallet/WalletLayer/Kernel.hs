{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Maybe (fromJust)
import           Data.Time.Units (Second)
import           System.Wlog (Severity (Debug))

import           Pos.Chain.Block (Blund, Undo (..))
import           Pos.Core (Address, Coin)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))

import           Cardano.Wallet.API.V1.Types (Payment (..),
                     PaymentDistribution (..), PaymentSource (..),
                     WalletId (..), unV1)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import           Cardano.Wallet.Kernel.ChainState (dummyChainBrief)
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, newOptions)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     fromRawResolvedBlock)
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     EstimateFeesError (..), NewPaymentError (..),
                     PassiveWalletLayer (..), WalletLayerError (..))

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logFunction keystore rocksDB f =
    Kernel.bracketPassiveWallet logFunction keystore rocksDB $ \w -> do
      let wai = Actions.WalletActionInterp
                 { Actions.applyBlocks = \blunds ->
                     Kernel.applyBlocks w
                        (OldestFirst (mapMaybe blundToResolvedBlock
                           (toList (getOldestFirst blunds))))
                 , Actions.switchToFork = \_ _ ->
                     logFunction Debug "<switchToFork>"
                 , Actions.emit = logFunction Debug }
      Actions.withWalletWorker wai $ \invoke -> do
         f (passiveWalletLayer w invoke) w
  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> STM ())
                       -> PassiveWalletLayer n
    passiveWalletLayer wallet invoke =
        let invokeIO :: forall m'. MonadIO m' => Actions.WalletAction Blund -> m' ()
            invokeIO = liftIO . STM.atomically . invoke
        in PassiveWalletLayer
            { _pwlCreateWallet          = Wallets.createWallet wallet

            , _pwlGetWallets            = do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    return (Wallets.getWallets snapshot)
            , _pwlGetWallet             =
                \walletId -> do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    return (Wallets.getWallet snapshot walletId)
            , _pwlUpdateWallet          = Wallets.updateWallet wallet
            , _pwlUpdateWalletPassword  = Wallets.updateWalletPassword wallet
            , _pwlDeleteWallet          = Wallets.deleteWallet wallet

            , _pwlCreateAccount = Accounts.createAccount wallet
            , _pwlGetAccounts   =
                \walletId -> do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    return (Accounts.getAccounts snapshot walletId)
            , _pwlGetAccount    =
                \walletId accountIndex -> do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    return (Accounts.getAccount snapshot walletId accountIndex)
            , _pwlUpdateAccount  = Accounts.updateAccount wallet
            , _pwlDeleteAccount  = Accounts.deleteAccount wallet

            , _pwlCreateAddress  = Addresses.createAddress wallet
            , _pwlGetAddresses   = error "Not implemented!"

            , _pwlApplyBlocks    = invokeIO . Actions.ApplyBlocks
            , _pwlRollbackBlocks = invokeIO . Actions.RollbackBlocks
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock dummyChainBrief
            $ UnsafeRawResolvedBlock mainBlock spentOutputs'
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
    Kernel.bracketActiveWallet pm passiveWallet walletDiffusion $ \activeWallet -> do
        bracket
          (return (activeWalletLayer activeWallet))
          (\_ -> return ())
          (flip runActiveLayer activeWallet)
  where

    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer activeWallet = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer

        -- | Generates a new transaction @and submit it as pending@.
        , pay = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $ do
                  (opts, accountId, payees) <-
                       liftIO $ setupPayment grouping
                                             regulation
                                             payment
                  res <- liftIO $ Kernel.pay activeWallet
                                             spendingPassword
                                             opts
                                             accountId
                                             payees
                  case res of
                       Left e   -> return . Left . NewPaymentError $ e
                       Right tx -> return . Right $ tx

        -- | Estimates the fees for a payment.
        , estimateFees = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
                  (opts, accountId, payees) <-
                      liftIO $ setupPayment grouping
                                            regulation
                                            payment
                  fees <- liftIO $ Kernel.estimateFees activeWallet
                                                       spendingPassword
                                                       opts
                                                       accountId
                                                       payees
                  case fees of
                       Left e  -> return . Left  . EstimateFeesError $ e
                       Right f -> return . Right $ f
        }


-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: InputGrouping
             -> ExpenseRegulation
             -> Payment
             -> IO ( CoinSelectionOptions
                   , HD.HdAccountId
                   , NonEmpty (Address, Coin)
                   )
setupPayment grouping regulation payment = do

    let (WalletId wId) = psWalletId . pmtSource $ payment

    hdRootId  <- case Core.decodeTextAddress wId of
                     Left e  -> throwM (InvalidAddressConversionFailed e)
                     Right a -> return (HD.HdRootId . InDb $ a)
    let opts = (newOptions Kernel.cardanoFee) {
               csoExpenseRegulation = regulation
             , csoInputGrouping     = grouping
             }
        accountIndex   = HD.HdAccountIx (psAccountIndex . pmtSource $ payment)
        accountId = HD.HdAccountId {
                    _hdAccountIdParent = hdRootId
                  , _hdAccountIdIx     = accountIndex
                  }
        payees    =  (\(PaymentDistribution a c) -> (unV1 a, unV1 c))
                 <$> (pmtDestinations payment)

    return (opts , accountId , payees)
