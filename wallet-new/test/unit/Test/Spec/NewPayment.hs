{-# LANGUAGE TypeApplications #-}
module Test.Spec.NewPayment (spec) where

import           Universum

import           Control.Lens (to)

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import           Data.Acid (update)
import           Formatting (build, formatToString, sformat)

import           Pos.Core (Address, Coin (..), IsBootstrapEraAddr (..),
                     deriveLvl2KeyPair, mkCoin)
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey, ShouldCheckPassphrase (..),
                     safeDeterministicKeyGen)

import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..), genPayee, genUtxoWithAtLeast)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation (..),
                     InputGrouping (..), newOptions)
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdAddressIx (..), HdRootId (..),
                     WalletName (..), eskToHdRootId, hdAccountIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.Internal (ActiveWallet, PassiveWallet,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.MonadDBReadAdaptor (rocksDBNotAvailable)
import qualified Cardano.Wallet.Kernel.PrefilterTx as Kernel
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import qualified Test.Spec.Fixture as Fixture
import           Util.Buildable (ShowThroughBuild (..))

import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Handlers
import           Control.Monad.Except (runExceptT)
import           Servant.Server


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    , fixturePayees    :: NonEmpty (Address, Coin)
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: InitialBalance
                -> Pay
                -> Fixture.GenActiveWalletFixture Fixture
prepareFixtures initialBalance toPay = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    utxo   <- pick (genUtxoWithAtLeast initialBalance)
    -- Override all the addresses of the random Utxo with something meaningful,
    -- i.e. with 'Address'(es) generated in a principled way, and not random.
    utxo' <- foldlM (\acc (txIn, (TxOutAux (TxOut _ coin))) -> do
                        newIndex <- deriveIndex (pick . choose) HdAddressIx HardDerivation

                        let Just (addr, _) = deriveLvl2KeyPair (IsBootstrapEraAddr True)
                                                               (ShouldCheckPassphrase True)
                                                               mempty
                                                               esk
                                                               (newAccountId ^. hdAccountIdIx . to getHdAccountIx)
                                                               (getHdAddressIx newIndex)
                        return $ M.insert txIn (TxOutAux (TxOut addr coin)) acc
                    ) M.empty (M.toList utxo)
    payees <- fmap (\(TxOut addr coin) -> (addr, coin)) <$> pick (genPayee utxo toPay)

    return $ \keystore aw -> do
        liftIO $ Keystore.insert (WalletIdHdRnd newRootId) esk keystore
        let pw = Kernel.walletPassive aw

        let accounts = Kernel.prefilterUtxo newRootId esk utxo'

        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdRnd newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         , fixturePayees = payees
                         }

withFixture :: MonadIO m
            => InitialBalance
            -> Pay
            -> (  Keystore.Keystore
               -> ActiveWalletLayer m
               -> ActiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture initialBalance toPay cc =
    Fixture.withActiveWalletFixture (prepareFixtures initialBalance toPay) cc

-- | A constant fee calculation.
constantFee :: Int -> NonEmpty Coin -> Coin
constantFee _ _ = mkCoin 10

-- | Helper function to facilitate payments via the Layer or Servant.
withPayment :: MonadIO n
            => InitialBalance
            -- ^ How big the wallet Utxo must be
            -> Pay
            -- ^ How big the payment must be
            -> (ActiveWalletLayer n -> V1.Payment -> IO ())
            -- ^ The action to run.
            -> PropertyM IO ()
withPayment initialBalance toPay action = do
    withFixture initialBalance toPay $ \keystore activeLayer _ Fixture{..} -> do
        liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
        let (AccountIdHdRnd hdAccountId)  = fixtureAccountId
        let (HdRootId (InDb rootAddress)) = fixtureHdRootId
        let sourceWallet = V1.WalletId (sformat build rootAddress)
        let accountIndex = hdAccountId ^. hdAccountIdIx . to getHdAccountIx
        let destinations =
                fmap (\(addr, coin) -> V1.PaymentDistribution (V1.V1 addr) (V1.V1 coin)
                     ) fixturePayees
        let newPayment = V1.Payment {
                         pmtSource           = V1.PaymentSource sourceWallet accountIndex
                       , pmtDestinations     = destinations
                       , pmtGroupingPolicy   = Nothing
                       , pmtSpendingPassword = Nothing
                       }
        action activeLayer newPayment

spec :: Spec
spec = describe "NewPayment" $ do

    describe "Generating a new payment (wallet layer)" $ do

        prop "pay works (realSigner, SenderPaysFee)" $ withMaxSuccess 50 $ do
            monadicIO $
                withPayment (InitialADA 10000) (PayLovelace 10) $ \activeLayer newPayment -> do
                    res <- liftIO ((WalletLayer.pay activeLayer) mempty
                                                                 IgnoreGrouping
                                                                 SenderPaysFee
                                                                 newPayment
                                  )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Generating a new payment (kernel)" $ do
        prop "newTransaction works (real signer, SenderPaysFee)" $ withMaxSuccess 50 $ do
            monadicIO $
                withFixture @IO (InitialADA 10000) (PayLovelace 10) $ \_ _ aw Fixture{..} -> do
                    let opts = (newOptions Kernel.cardanoFee) {
                               csoExpenseRegulation = SenderPaysFee
                             , csoInputGrouping     = IgnoreGrouping
                             }
                    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                    (res, _) <- liftIO (Kernel.newTransaction aw
                                                              mempty
                                                              opts
                                                              hdAccountId
                                                              fixturePayees
                                       )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

        prop "newTransaction works (ReceiverPaysFee)" $ withMaxSuccess 50 $ do
            monadicIO $
                withFixture @IO (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                    let opts = (newOptions Kernel.cardanoFee) {
                               csoExpenseRegulation = ReceiverPaysFee
                             , csoInputGrouping     = IgnoreGrouping
                             }
                    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                    (res, _) <- liftIO (Kernel.newTransaction aw
                                                              mempty
                                                              opts
                                                              hdAccountId
                                                              fixturePayees
                                       )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Generating a new payment (Servant)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $
            monadicIO $
                withPayment (InitialADA 1000) (PayADA 1) $ \activeLayer newPayment -> do
                    res <- liftIO (runExceptT . runHandler' $ Handlers.newTransaction activeLayer newPayment)
                    liftIO ((bimap identity STB res) `shouldSatisfy` isRight)

    describe "EstimateFees" $ do

        describe "Estimating fees (wallet layer)" $ do

            prop "estimating fees works (SenderPaysFee)" $ withMaxSuccess 50 $ do
                monadicIO $
                    withPayment (InitialADA 10000) (PayLovelace 10) $ \activeLayer newPayment -> do
                        res <- liftIO ((WalletLayer.estimateFees activeLayer) mempty
                                                                              IgnoreGrouping
                                                                              SenderPaysFee
                                                                              newPayment
                                      )
                        case res of
                             Left e  -> fail (formatToString build e)
                             Right fee ->
                                 fee `shouldSatisfy` (> (Coin 0))

        describe "Estimating fees (kernel)" $ do
            prop "estimating fees works (SenderPaysFee)" $ withMaxSuccess 50 $
                monadicIO $
                    withFixture @IO (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        let opts = (newOptions constantFee) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }
                        let (AccountIdHdRnd hdAccountId) = fixtureAccountId

                        res <- liftIO (Kernel.estimateFees aw
                                                           mempty
                                                           opts
                                                           hdAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldBe` Coin 10

            prop "estimating fees works (kernel, ReceiverPaysFee)" $ withMaxSuccess 50 $
                monadicIO $
                    withFixture @IO (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        let opts = (newOptions constantFee) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }
                        let (AccountIdHdRnd hdAccountId) = fixtureAccountId

                        res <- liftIO (Kernel.estimateFees aw
                                                           mempty
                                                           opts
                                                           hdAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldBe` Coin 10

            prop "estimating fees works (kernel, SenderPaysFee, cardanoFee)" $ withMaxSuccess 50 $
                monadicIO $
                    withFixture @IO (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        let opts = (newOptions Kernel.cardanoFee) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }
                        let (AccountIdHdRnd hdAccountId) = fixtureAccountId

                        res <- liftIO (Kernel.estimateFees aw
                                                           mempty
                                                           opts
                                                           hdAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldSatisfy` (> (Coin 0))

        describe "Estimating fees (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $
                monadicIO $
                    withPayment (InitialADA 1000) (PayADA 1) $ \activeLayer newPayment -> do
                        res <- liftIO (runExceptT . runHandler' $ Handlers.estimateFees activeLayer newPayment)
                        liftIO ((bimap identity STB res) `shouldSatisfy` isRight)
