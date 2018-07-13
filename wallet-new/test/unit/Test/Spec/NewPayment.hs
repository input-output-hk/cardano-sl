{-# LANGUAGE TypeApplications #-}
module Test.Spec.NewPayment (spec) where

import           Universum

import           Control.Lens (to)

import           Test.Hspec (Spec, describe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import           Data.Acid (update)
import           Formatting (build, sformat)
import           System.Wlog (Severity)
import           Test.Pos.Configuration (withDefConfiguration)

import           Pos.Core (Address, Coin, IsBootstrapEraAddr (..), TxOut (..),
                     TxOutAux (..), deriveLvl2KeyPair)
import           Pos.Crypto (EncryptedSecretKey, ShouldCheckPassphrase (..),
                     safeDeterministicKeyGen)

import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..), genPayee, genUtxoWithAtLeast)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
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
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Cardano.Wallet.Kernel.Internal (ActiveWallet, PassiveWallet,
                     wallets)
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.PrefilterTx as Kernel
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = return ()

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    , fixtureAw        :: ActiveWallet
    , fixturePayees    :: NonEmpty (Address, Coin)
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: PropertyM IO (Keystore -> ActiveWallet -> IO Fixture)
prepareFixtures = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    utxo   <- pick (genUtxoWithAtLeast (InitialADA 100))
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
    payees <- fmap (\(TxOut addr coin) -> (addr, coin)) <$> pick (genPayee utxo (PayADA 10))

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
                         , fixtureAw  = aw
                         , fixturePayees = payees
                         }

withFixture :: MonadIO m
            => (Keystore.Keystore -> ActiveWalletLayer m -> Fixture -> IO a) -> PropertyM IO a
withFixture cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \passiveLayer passiveWallet -> do
            withDefConfiguration $ \pm -> do
                WalletLayer.bracketKernelActiveWallet pm passiveLayer passiveWallet diffusion $ \activeLayer activeWallet -> do
                    fixtures <- generateFixtures keystore activeWallet
                    cc keystore activeLayer fixtures
    where
        diffusion :: Kernel.WalletDiffusion
        diffusion =  Kernel.WalletDiffusion {
            walletSendTx = \_tx -> return False
          }

genChangeAddr :: AccountId -> PassiveWallet -> IO Address
genChangeAddr accountId pw = do
    res <- Kernel.createAddress mempty accountId pw
    case res of
         Right addr -> pure addr
         Left err   -> throwM err

spec :: Spec
spec = describe "NewPayment" $ do

    describe "Generating a new payment (wallet layer)" $ do

        prop "pay works (SenderPaysFee)" $ do
            monadicIO $ do
                withFixture @IO $ \_ activeLayer Fixture{..} -> do
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
                    res <- liftIO ((WalletLayer.pay activeLayer) mempty
                                                                 IgnoreGrouping
                                                                 SenderPaysFee
                                                                 newPayment
                                  )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Generating a new payment (kernel)" $ do
        prop "newTransaction works " $ do
            monadicIO $ do
                withFixture @IO $ \_ _ Fixture{..} -> do
                    allWallets <- Kernel.hdWallets <$> Kernel.getWalletSnapshot fixturePw
                    let opts = (newOptions Kernel.cardanoFee) {
                               csoExpenseRegulation = SenderPaysFee
                             , csoInputGrouping     = IgnoreGrouping
                             }
                    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                    res <- liftIO (Kernel.newTransaction fixtureAw
                                                         (genChangeAddr fixtureAccountId fixturePw)
                                                         (Kernel.mkSigner mempty (Just fixtureESK) allWallets)
                                                         opts
                                                         hdAccountId
                                                         fixturePayees
                                  )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)
{--
    describe "Generating a new payment (Servant)" $ do
        prop "works as expected in the happy path scenario" $ do
            monadicIO $
                withFixture $ \keystore layer Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                        req = V1.NewAddress Nothing accIdx (V1.WalletId wId)
                    res <- liftIO (runExceptT . runHandler' $ Handlers.newAddress layer req)
                    liftIO ((bimap identity STB res) `shouldSatisfy` isRight)
--}
