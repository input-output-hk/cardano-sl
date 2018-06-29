{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Control.Lens (to)
import           Data.Coerce (coerce)
import           Data.Default (def)
import           Data.Maybe (fromJust)
import           Data.Time.Units (Second)
import           System.Wlog (Severity (Debug))

import           Pos.Block.Types (Blund, Undo (..))

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import qualified Cardano.Wallet.Kernel.Transactions as Kernel

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Read
                     (readHdAddressByCardanoAddress)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import qualified Cardano.Wallet.Kernel.Internal as Internal
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedBlock (..), WalletId (WalletIdHdRnd),
                     fromRawResolvedBlock)
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     CreateAddressError (..), EstimateFeesError (..),
                     NewPaymentError (..), PassiveWalletLayer (..),
                     WalletLayerError (..))

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, estimateCardanoFee, newOptions)
import           Cardano.Wallet.Kernel.CoinSelection.Generic
                     (CoinSelHardErr (..))
import           Pos.Core (decodeTextAddress)

import           Pos.Core (Address, Coin)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (ShouldCheckPassphrase (..),
                     safeDeterministicKeyGen)
import           Pos.Util.Mnemonic (Mnemonic, mnemonicToSeed)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Data.Map.Strict as Map
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (EstimatedFees (..), Payment (..),
                     PaymentDistribution (..), PaymentSource (..), V1 (..),
                     WalletId (..), unV1)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logFunction keystore f =
    Kernel.bracketPassiveWallet logFunction keystore $ \w -> do

      -- Create the wallet worker and its communication endpoint `invoke`.
      bracket (liftIO $ Actions.forkWalletWorker $ Actions.WalletActionInterp
                 { Actions.applyBlocks  =  \blunds ->
                     Kernel.applyBlocks w $
                         OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
                 , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
                 , Actions.emit         = logFunction Debug
                 }
              ) (\invoke -> liftIO (invoke Actions.Shutdown))
              $ \invoke -> do
                  -- TODO (temporary): build a sample wallet from a backup phrase
                  _ <- liftIO $ do
                    let (_, esk) = safeDeterministicKeyGen (mnemonicToSeed $ def @(Mnemonic 12)) emptyPassphrase
                    Kernel.createWalletHdRnd w walletName spendingPassword assuranceLevel esk Map.empty

                  f (passiveWalletLayer w invoke) w

  where
    -- TODO consider defaults
    walletName       = HD.WalletName "(new wallet)"
    spendingPassword = HD.NoSpendingPassword
    assuranceLevel   = HD.AssuranceLevelNormal

    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer n
    passiveWalletLayer wallet invoke =
        PassiveWalletLayer
            { _pwlCreateWallet   = error "Not implemented!"
            , _pwlGetWalletIds   = error "Not implemented!"
            , _pwlGetWallet      = error "Not implemented!"
            , _pwlUpdateWallet   = error "Not implemented!"
            , _pwlDeleteWallet   = error "Not implemented!"

            , _pwlCreateAccount  = error "Not implemented!"
            , _pwlGetAccounts    = error "Not implemented!"
            , _pwlGetAccount     = error "Not implemented!"
            , _pwlUpdateAccount  = error "Not implemented!"
            , _pwlDeleteAccount  = error "Not implemented!"

            , _pwlCreateAddress  =
                \(V1.NewAddress mbSpendingPassword accIdx (V1.WalletId wId)) -> do
                    liftIO $ limitExecutionTimeTo (30 :: Second) CreateAddressTimeLimitReached $ do
                        case decodeTextAddress wId of
                             Left _ ->
                                 return $ Left (CreateAddressAddressDecodingFailed wId)
                             Right rootAddr -> do
                                let hdRootId = HD.HdRootId . InDb $ rootAddr
                                let hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accIdx)
                                let passPhrase = maybe mempty coerce mbSpendingPassword
                                res <- liftIO $ Kernel.createAddress passPhrase
                                                                     (AccountIdHdRnd hdAccountId)
                                                                     wallet
                                case res of
                                     Right newAddr -> return (Right newAddr)
                                     Left  err     -> return (Left $ CreateAddressError err)
            , _pwlGetAddresses   = error "Not implemented!"

            , _pwlApplyBlocks    = liftIO . invoke . Actions.ApplyBlocks
            , _pwlRollbackBlocks = liftIO . invoke . Actions.RollbackBlocks
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock Nothing spentOutputs'
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
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet pm walletPassiveLayer passiveWallet walletDiffusion runActiveLayer =
    Kernel.bracketActiveWallet pm passiveWallet walletDiffusion $ \activeWallet -> do
        bracket
          (return (activeWalletLayer activeWallet))
          (\_ -> return ())
          runActiveLayer
  where

    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer activeWallet = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer

        -- | Generates a new transaction @and submit it as pending@.
        , pay = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $ do
                  let pw = Kernel.walletPassive activeWallet
                  snapshot <- liftIO (Kernel.getWalletSnapshot pw)
                  let keystore = Kernel.walletPassive activeWallet ^. Internal.walletKeystore
                  (genChangeAddr, mkSigner, opts, accountId, payees) <-
                       liftIO $ setupPayment pw
                                             (Kernel.hdWallets snapshot)
                                             keystore
                                             spendingPassword
                                             grouping
                                             regulation
                                             payment
                  res <- liftIO $ Kernel.pay activeWallet
                                             genChangeAddr
                                             mkSigner
                                             opts
                                             accountId
                                             payees
                  case res of
                       Left e   -> return . Left . NewPaymentError $ e
                       Right tx -> return . Right $ tx

        -- | Estimates the fees for a payment.
        , estimateFees = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
                  let pw = Kernel.walletPassive activeWallet
                  snapshot <- liftIO (Kernel.getWalletSnapshot pw)
                  let keystore = Kernel.walletPassive activeWallet ^. Internal.walletKeystore
                  (genChangeAddr, mkSigner, opts, accountId, payees) <-
                      liftIO $ setupPayment pw
                                            (Kernel.hdWallets snapshot)
                                            keystore
                                            spendingPassword
                                            grouping
                                            regulation
                                            payment
                  fees <- liftIO $ Kernel.estimateFees activeWallet
                                                       genChangeAddr
                                                       mkSigner
                                                       opts
                                                       accountId
                                                       payees
                  case fees of
                       Left e  -> return . Left  . EstimateFeesError $ e
                       Right f -> return . Right . EstimatedFees . V1 $ f
        }


setupPayment :: Kernel.PassiveWallet
             -> HD.HdWallets
             -> Keystore
             -> PassPhrase
             -> InputGrouping
             -> ExpenseRegulation
             -> Payment
             -> IO ( IO Address
                   , Address -> Either CoinSelHardErr SafeSigner
                   , CoinSelectionOptions
                   , HD.HdAccountId
                   , NonEmpty (Address, Coin)
                   )
setupPayment pw wallets keystore spendingPassword grouping regulation payment = do
    let (WalletId wId) = psWalletId . pmtSource $ payment
    hdRootId  <- case Core.decodeTextAddress wId of
                     Left e  -> throwM (InvalidAddressConversionFailed e)
                     Right a -> return (HD.HdRootId . InDb $ a)
    let opts = (newOptions cardanoFee) {
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

    let genChangeAddr = do
            res <- Kernel.createAddress spendingPassword (AccountIdHdRnd accountId) pw
            case res of
                 Right addr -> pure addr
                 Left err   -> throwM err

    -- | NOTE(adn): For V1 'WalletId' types, we do not have anything different
    -- than an HD random ID, but this won't be true in the future anymore, but
    -- that's currently the only schema supported by the keystore.
    mbEsk <- Keystore.lookup (WalletIdHdRnd hdRootId) keystore
    return (genChangeAddr, mkSigner mbEsk wallets, opts, accountId, payees)

  where

    -- NOTE(adn) At the moment we are passing
    -- the full set of Hd wallets as input, which means our lookup function
    -- for an 'HdAddress' can span across the whole wallets and can be very
    -- costly. However, in the way the 'DB' is modelled at the moment, we
    -- store the addresses in a \"flat\" representation (i.e. in an 'IxSet'),
    -- so indexing directly by 'Core.Address' might not be any less efficient
    -- than performing two lookups, one on 'HdAccountId' followed by one on
    -- 'Core.Address'.
    mkSigner :: Maybe EncryptedSecretKey
             -> HD.HdWallets
             -> Address
             -> Either CoinSelHardErr SafeSigner
    mkSigner Nothing _ addr = Left (CoinSelHardErrAddressNotOwned addr)
    mkSigner (Just esk) hdWallets addr =
        case readHdAddressByCardanoAddress addr hdWallets of
            Left _ -> Left (CoinSelHardErrAddressNotOwned addr)
            Right hdAddr ->
                let addressIndex = hdAddr ^. HD.hdAddressId
                                           . HD.hdAddressIdIx
                                           . to HD.getHdAddressIx
                    accountIndex = hdAddr ^. HD.hdAddressId
                                           . HD.hdAddressIdParent
                                           . HD.hdAccountIdIx
                                           . to HD.getHdAccountIx
                    res = Core.deriveLvl2KeyPair (Core.IsBootstrapEraAddr True)
                                                 (ShouldCheckPassphrase False)
                                                 spendingPassword
                                                 esk
                                                 accountIndex
                                                 addressIndex
                in case res of
                     Just (a, _) | a == addr ->
                         Right (SafeSigner esk spendingPassword)
                     _                   ->
                         Left (CoinSelHardErrAddressNotOwned addr)

    -- | An hopefully-accurate estimate of the Tx fees in Cardano.
    cardanoFee :: Int -> NonEmpty Coin -> Coin
    cardanoFee inputs outputs = Core.mkCoin $
        estimateCardanoFee linearFeePolicy inputs (toList $ fmap Core.getCoin outputs)
        where
          linearFeePolicy = Core.TxSizeLinear (Core.Coeff 155381) (Core.Coeff 43.946)
